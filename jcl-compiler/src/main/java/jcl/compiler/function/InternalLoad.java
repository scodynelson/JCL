package jcl.compiler.function;

import java.io.File;
import java.lang.reflect.Method;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Enumeration;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.jar.Attributes;
import java.util.jar.Manifest;

import jcl.compiler.classloaders.LoaderClassLoader;
import jcl.lang.BooleanStruct;
import jcl.lang.FileStreamStruct;
import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.PackageStruct;
import jcl.lang.PathnameStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.StringStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.TStruct;
import jcl.lang.condition.exception.FileErrorException;
import jcl.lang.pathname.PathnameVersion;
import jcl.lang.pathname.PathnameVersionComponentType;
import jcl.lang.statics.CompilerVariables;
import jcl.lang.statics.PackageVariables;
import jcl.lang.statics.PathnameVariables;
import jcl.lang.statics.ReaderVariables;
import jcl.reader.InternalRead;
import lombok.experimental.UtilityClass;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

@Slf4j
@UtilityClass
public final class InternalLoad {

	private static final String MANIFEST_RESOURCE_LOCATION = "META-INF/MANIFEST.MF";

	private static final Map<String, String> LISP_MODULE_TO_MAIN_CLASS_MAP = new ConcurrentHashMap<>();

	public static void autoLoadJavaModules() {
		try {
			ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
			if (classLoader == null) {
				classLoader = ClassLoader.getSystemClassLoader();
			}

			final Enumeration<URL> urls = (classLoader != null) ?
			                              classLoader.getResources(MANIFEST_RESOURCE_LOCATION) :
			                              ClassLoader.getSystemResources(MANIFEST_RESOURCE_LOCATION);

			while (urls.hasMoreElements()) {
				final URL url = urls.nextElement();

				final Manifest manifest = new Manifest(url.openStream());
				final Attributes mainAttributes = manifest.getMainAttributes();

				if (mainAttributes.containsKey(InternalCompile.LISP_MODULE_NAME)) {
					final String mainClassName = mainAttributes.getValue(Attributes.Name.MAIN_CLASS);
					final String currentLispModuleName = mainAttributes.getValue(InternalCompile.LISP_MODULE_NAME);

					LISP_MODULE_TO_MAIN_CLASS_MAP.put(currentLispModuleName, mainClassName);

					final boolean compileVerbose = CompilerVariables.COMPILE_VERBOSE.getVariableValue().toJavaPBoolean();
					final boolean loadVerbose = CompilerVariables.LOAD_VERBOSE.getVariableValue().toJavaPBoolean();
					if (compileVerbose || loadVerbose) {
						// TODO: is this verbose check correct???
						log.info("; Loading Module: {}", currentLispModuleName);
					}

					final ClassLoader systemClassLoader = ClassLoader.getSystemClassLoader();
					loadMainClass(systemClassLoader, mainClassName);
				}
			}
		} catch (final Exception ex) {
			log.error("Error auto-loading compiled files.", ex);
		}
	}

	public static LispStruct loadJavaModule(final StringStruct moduleName) {
		final String lispModuleName = moduleName.toJavaString();

		try {
			ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
			if (classLoader == null) {
				classLoader = ClassLoader.getSystemClassLoader();
			}

			if (LISP_MODULE_TO_MAIN_CLASS_MAP.containsKey(lispModuleName)) {
				final String mainClassName = LISP_MODULE_TO_MAIN_CLASS_MAP.get(lispModuleName);
				return loadMainClass(classLoader, mainClassName);
			}

			final Enumeration<URL> urls = (classLoader != null) ?
			                              classLoader.getResources(MANIFEST_RESOURCE_LOCATION) :
			                              ClassLoader.getSystemResources(MANIFEST_RESOURCE_LOCATION);

			while (urls.hasMoreElements()) {
				final URL url = urls.nextElement();

				final Manifest manifest = new Manifest(url.openStream());
				final Attributes mainAttributes = manifest.getMainAttributes();

				if (mainAttributes.containsKey(InternalCompile.LISP_MODULE_NAME)) {
					final String mainClassName = mainAttributes.getValue(Attributes.Name.MAIN_CLASS);
					final String currentLispModuleName = mainAttributes.getValue(InternalCompile.LISP_MODULE_NAME);

					LISP_MODULE_TO_MAIN_CLASS_MAP.put(currentLispModuleName, mainClassName);

					if (lispModuleName.equals(currentLispModuleName)) {
						final boolean compileVerbose = CompilerVariables.COMPILE_VERBOSE.getVariableValue().toJavaPBoolean();
						final boolean loadVerbose = CompilerVariables.LOAD_VERBOSE.getVariableValue().toJavaPBoolean();
						if (compileVerbose || loadVerbose) {
							// TODO: is this verbose check correct???
							log.info("; Loading Module: {}", lispModuleName);
						}

						final ClassLoader systemClassLoader = ClassLoader.getSystemClassLoader();
						return loadMainClass(systemClassLoader, mainClassName);
					}
				}
			}

			log.warn("Module-name '{}' failed to match any available compiled lisp modules on the classpath.", lispModuleName);
			return NILStruct.INSTANCE;
		} catch (final Exception ex) {
			log.error("Error loading main definition for compiled file: '{}'", lispModuleName, ex);
			return NILStruct.INSTANCE;
		}
	}

	private static LispStruct loadMainClass(final ClassLoader classLoader, final String mainClassName) throws Exception {
		classLoader.loadClass(mainClassName);

		final Class<?> classLoaded = Class.forName(mainClassName, true, classLoader);

		final Method mainMethod = classLoaded.getMethod("main", String[].class);
		final String[] params = null;
		mainMethod.invoke(null, (Object) params);
		return TStruct.INSTANCE;
	}

	public static LispStruct load(final LispStruct filespec, final BooleanStruct verboseVal, final BooleanStruct printVal,
	                              final BooleanStruct ifDoesNotExistVal, final LispStruct externalFormat) {
		final boolean verbose = verboseVal.toJavaPBoolean();
		final boolean print = printVal.toJavaPBoolean();
		final boolean ifDoesNotExist = ifDoesNotExistVal.toJavaPBoolean();

		FileStreamStruct filespecFileStream = null;

		final Path filespecPath;
		final PathnameStruct filespecPathname;

		// NOTE: optimizations if the filespec is already a FileStreamStruct
		if (filespec instanceof FileStreamStruct) {
			filespecFileStream = (FileStreamStruct) filespec;
			filespecPath = filespecFileStream.getPath();
			filespecPathname = PathnameStruct.toPathname(filespecPath);
		} else {
			final PathnameStruct filespecAsPathname = PathnameStruct.toPathname(filespec);
			final PathnameStruct defaultPathspec = PathnameVariables.DEFAULT_PATHNAME_DEFAULTS.getVariableValue();
			final PathnameVersion nilVersion = new PathnameVersion(PathnameVersionComponentType.NIL);
			filespecPathname = PathnameStruct.mergePathnames(filespecAsPathname, defaultPathspec, nilVersion);
			final File pathnameFile = new File(filespecPathname.getNamestring());
			filespecPath = pathnameFile.toPath();
		}

		final boolean filespecNotExists = Files.notExists(filespecPath);
		if (filespecNotExists && ifDoesNotExist) {
			throw new FileErrorException("Filespec provided to LOAD does not exist: " + filespecPath, filespecFileStream);
		}
		if (filespecNotExists) {
			return NILStruct.INSTANCE;
		}

		final LispStruct previousLoadPathname = CompilerVariables.LOAD_PATHNAME.getValue();
		final LispStruct previousLoadTruename = CompilerVariables.LOAD_TRUENAME.getValue();

		CompilerVariables.COMPILE_FILE_PATHNAME.setValue(filespecPathname);
		final Path filespecAbsolutePath = filespecPath.toAbsolutePath();
		final PathnameStruct filespecTruename = PathnameStruct.toPathname(filespecAbsolutePath);
		CompilerVariables.COMPILE_FILE_TRUENAME.setValue(filespecTruename);

		final ReadtableStruct previousReadtable = ReaderVariables.READTABLE.getVariableValue();
		final PackageStruct previousPackage = PackageVariables.PACKAGE.getVariableValue();

		try {
			final String filespecNamestring = filespecPath.toString();
			if (StringUtils.endsWithIgnoreCase(filespecNamestring, ".lar") || StringUtils.endsWithIgnoreCase(filespecNamestring, ".jar")) {
				return loadCompiledCode(filespecPath, verbose, print);
			} else if (StringUtils.endsWithIgnoreCase(filespecNamestring, ".lsp") || StringUtils.endsWithIgnoreCase(filespecNamestring, ".lisp")) {
				if (filespecFileStream == null) {
					filespecFileStream = FileStreamStruct.toFileStream(filespecPath);
				}
				return loadSourceCode(filespecFileStream, filespecPath, verbose, print);
			} else {
				throw new FileErrorException("Cannot LOAD file with unsupported extension: " + filespecPath, filespecFileStream);
			}
		} finally {
			CompilerVariables.LOAD_TRUENAME.setValue(previousLoadTruename);
			CompilerVariables.LOAD_PATHNAME.setValue(previousLoadPathname);

			PackageVariables.PACKAGE.setValue(previousPackage);
			ReaderVariables.READTABLE.setValue(previousReadtable);
		}
	}

	private static LispStruct loadSourceCode(final FileStreamStruct filespecFileStream, final Path filespecPath,
	                                         final boolean verbose, final boolean print) {

		if (verbose) {
			log.info("; Loading '{}'", filespecPath);
		}

		LispStruct form;
		do {
			form = InternalRead.read(filespecFileStream, NILStruct.INSTANCE, null, NILStruct.INSTANCE);
			if (form == null) {
				continue;
			}

			final LispStruct evaluatedForm = InternalEval.eval(form);
			if (print) {
				log.info("; {}", evaluatedForm);
			}
		} while (form != null);

		return TStruct.INSTANCE;
	}

	private static LispStruct loadCompiledCode(final Path filespecPath, final boolean verbose, final boolean print) {

		try {
			final LoaderClassLoader cl = new LoaderClassLoader(filespecPath, verbose, print);
			final Class<?> classLoaded = cl.loadMainClass();

			if (classLoaded == null) {
				return NILStruct.INSTANCE;
			} else {
				final FunctionStruct function = (FunctionStruct) classLoaded.getConstructor().newInstance();
				final SymbolStruct functionSymbol = function.getFunctionSymbol();
				functionSymbol.setFunction(function);
				return function.apply();
			}
		} catch (final FileErrorException fee) {
			log.error(fee.getMessage(), fee.getCause());
			return NILStruct.INSTANCE;
		} catch (final Exception ex) {
			log.error("Error loading main definition for compiled file: '{}'", filespecPath, ex);
			return NILStruct.INSTANCE;
		}
	}
}
