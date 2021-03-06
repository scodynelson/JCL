package jcl.compiler.function;

import java.io.File;
import java.lang.reflect.Method;
import java.net.URL;
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
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.statics.CompilerVariables;
import jcl.lang.statics.PackageVariables;
import jcl.lang.statics.ReaderVariables;
import jcl.reader.InternalRead;
import lombok.experimental.UtilityClass;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.StringUtils;

@Log4j2
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

		final PathnameStruct filespecPathname;

		// NOTE: optimizations if the filespec is already a FileStreamStruct
		if (filespec instanceof FileStreamStruct) {
			filespecFileStream = (FileStreamStruct) filespec;
			filespecPathname = filespecFileStream.toPathname();
		} else {
//			final PathnameStruct filespecAsPathname = PathnameStruct.toPathname(filespec);
//			final PathnameStruct defaultPathspec = PathnameVariables.DEFAULT_PATHNAME_DEFAULTS.getVariableValue();
//			filespecPathname = PathnameStructs.mergePathnames(filespecAsPathname, defaultPathspec);
			filespecPathname = PathnameStruct.fromDesignator(filespec);
		}

		final File pathnameFile = new File(filespecPathname.namestring());

		if (!pathnameFile.exists() && ifDoesNotExist) {
			throw new FileErrorException("Filespec provided to LOAD does not exist: " + pathnameFile, filespecFileStream);
		}
		if (!pathnameFile.exists()) {
			return NILStruct.INSTANCE;
		}

		final LispStruct previousLoadPathname = CompilerVariables.LOAD_PATHNAME.getValue();
		final LispStruct previousLoadTruename = CompilerVariables.LOAD_TRUENAME.getValue();

		CompilerVariables.COMPILE_FILE_PATHNAME.setValue(filespecPathname);
		final PathnameStruct filespecTruename = PathnameStruct.toPathname(pathnameFile.toURI().toString());
		CompilerVariables.COMPILE_FILE_TRUENAME.setValue(filespecTruename);

		final ReadtableStruct previousReadtable = ReaderVariables.READTABLE.getVariableValue();
		final PackageStruct previousPackage = PackageVariables.PACKAGE.getVariableValue();

		try {
			final String filespecNamestring = pathnameFile.toString();
			if (StringUtils.endsWithIgnoreCase(filespecNamestring, ".lar") || StringUtils.endsWithIgnoreCase(filespecNamestring, ".jar")) {
				return loadCompiledCode(pathnameFile, verbose, print);
			} else if (StringUtils.endsWithIgnoreCase(filespecNamestring, ".lsp") || StringUtils.endsWithIgnoreCase(filespecNamestring, ".lisp")) {
				if (filespecFileStream == null) {
					filespecFileStream = FileStreamStruct.toFileStream(
							filespecPathname, CommonLispSymbols.INPUT_KEYWORD, CommonLispSymbols.CHARACTER,
							CommonLispSymbols.NIL, CommonLispSymbols.NIL, CommonLispSymbols.DEFAULT_KEYWORD
					);
				}
				return loadSourceCode(filespecFileStream, pathnameFile, verbose, print);
			} else {
				throw new FileErrorException("Cannot LOAD file with unsupported extension: " + pathnameFile, filespecFileStream);
			}
		} finally {
			CompilerVariables.LOAD_TRUENAME.setValue(previousLoadTruename);
			CompilerVariables.LOAD_PATHNAME.setValue(previousLoadPathname);

			PackageVariables.PACKAGE.setValue(previousPackage);
			ReaderVariables.READTABLE.setValue(previousReadtable);
		}
	}

	private static LispStruct loadSourceCode(final FileStreamStruct filespecFileStream, final File pathnameFile,
	                                         final boolean verbose, final boolean print) {

		if (verbose) {
			log.info("; Loading '{}'", pathnameFile);
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

	private static LispStruct loadCompiledCode(final File pathnameFile, final boolean verbose, final boolean print) {

		try {
			final LoaderClassLoader cl = new LoaderClassLoader(pathnameFile, verbose, print);
			final Class<?> classLoaded = cl.loadMainClass();

			if (classLoaded == null) {
				return NILStruct.INSTANCE;
			} else {
				final Method mainMethod = classLoaded.getMethod("main", String[].class);
				final String[] params = null;
				mainMethod.invoke(null, (Object) params);
				return TStruct.INSTANCE;

// TODO: Should use main method or just have the main class be a Function??
//				final FunctionStruct function = (FunctionStruct) classLoaded.getConstructor().newInstance();
//				final SymbolStruct functionSymbol = function.getFunctionSymbol();
//				functionSymbol.setFunction(function);
//				return function.apply();
			}
		} catch (final FileErrorException fee) {
			log.error(fee.getMessage(), fee.getCause());
			return NILStruct.INSTANCE;
		} catch (final Exception ex) {
			log.error("Error loading main definition for compiled file: '{}'", pathnameFile, ex);
			return NILStruct.INSTANCE;
		}
	}
}
