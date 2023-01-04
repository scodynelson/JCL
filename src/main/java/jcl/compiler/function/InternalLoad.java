package jcl.compiler.function;

import java.io.File;
import java.lang.reflect.Method;
import java.net.URL;
import java.util.Enumeration;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.jar.Attributes;
import java.util.jar.Manifest;

import jcl.compiler.classloaders.LoaderClassLoader;
import jcl.lang.BooleanStruct;
import jcl.lang.FileStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.PackageStruct;
import jcl.lang.PathnameStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.StringStruct;
import jcl.lang.TStruct;
import jcl.lang.condition.exception.FileErrorException;
import jcl.lang.statics.CommonLispSymbols;
import jcl.reader.InternalRead;
import lombok.experimental.UtilityClass;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.lang3.StringUtils;

import static java.util.Map.entry;

@Log4j2
@UtilityClass
public final class InternalLoad {

	private static final String MANIFEST_RESOURCE_LOCATION = "META-INF/MANIFEST.MF";

	private static final Map<String, String> LISP_MODULE_TO_MAIN_CLASS_MAP = new ConcurrentHashMap<>();

	public static void autoLoadMainClasses() {
		if (!LISP_MODULE_TO_MAIN_CLASS_MAP.isEmpty()) {
			return;
		}

		// TODO: this manual loading of main classes isn't sustainable. Should be replaced with something better...
		try {
			final Map<String, String> mainClasses = Map.ofEntries(
					entry("base-macro-lambdas", "jcl.Base_macro_lambdas"),
					entry("sequences", "jcl.Sequences"),
					entry("macros", "jcl.Macros"),
					entry("iterators", "jcl.Iterators"),
					entry("conditions", "jcl.Conditions"),
					entry("characters", "jcl.Characters"),
					entry("pathnames", "jcl.Pathnames"),
					entry("symbols", "jcl.Symbols"),
					entry("strings", "jcl.Strings"),
					entry("streams", "jcl.Streams"),
					entry("reader", "jcl.Reader"),
					entry("packages", "jcl.Packages"),
					entry("lists", "jcl.Lists"),
					entry("numbers", "jcl.Numbers"),
					entry("hashtables", "jcl.Hashtables"),
					entry("setf", "jcl.Setf"),
					entry("files", "jcl.Files"),
					entry("environment", "jcl.Environment"),
					entry("structures", "jcl.Structures")
			);
			final List<String> modules = List.of(
					"base-macro-lambdas",
					"sequences",
					"macros",
					"iterators",
					"conditions",
					"characters",
					"pathnames",
					"symbols",
					"strings",
					"streams",
					"reader",
					"packages",
					"lists",
					"numbers",
					"hashtables",
					"setf",
					"files",
					"environment",
					"structures"
			);

			for (final String lispModuleName : modules) {
				final String mainClassName = mainClasses.get(lispModuleName);
				LISP_MODULE_TO_MAIN_CLASS_MAP.put(lispModuleName, mainClassName);

				final boolean compileVerbose = CommonLispSymbols.COMPILE_VERBOSE_VAR.getVariableValue().toJavaPBoolean();
				final boolean loadVerbose = CommonLispSymbols.LOAD_VERBOSE_VAR.getVariableValue().toJavaPBoolean();
				if (compileVerbose || loadVerbose) {
					// TODO: is this verbose check correct???
					log.info("; Loading Module: {}", lispModuleName);
				}

				final ClassLoader systemClassLoader = ClassLoader.getSystemClassLoader();
				loadMainClass(systemClassLoader, mainClassName);
			}
		} catch (final Exception ex) {
			log.error("Error auto-loading main classes.", ex);
		}
	}

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

					final boolean compileVerbose = CommonLispSymbols.COMPILE_VERBOSE_VAR.getVariableValue().toJavaPBoolean();
					final boolean loadVerbose = CommonLispSymbols.LOAD_VERBOSE_VAR.getVariableValue().toJavaPBoolean();
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
						final boolean compileVerbose = CommonLispSymbols.COMPILE_VERBOSE_VAR.getVariableValue().toJavaPBoolean();
						final boolean loadVerbose = CommonLispSymbols.LOAD_VERBOSE_VAR.getVariableValue().toJavaPBoolean();
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
			final PathnameStruct filespecAsPathname = PathnameStruct.fromDesignator(filespec);
			final PathnameStruct defaultPathspec = CommonLispSymbols.DEFAULT_PATHNAME_DEFAULTS_VAR.getVariableValue();
//			filespecPathname = PathnameStructs.mergePathnames(filespecAsPathname, defaultPathspec);

			// TODO: temp hack for merge
			filespecPathname = PathnameStruct.toPathname(
					filespecAsPathname.pathnameHost().eq(NILStruct.INSTANCE)
					? defaultPathspec.pathnameHost() : filespecAsPathname.pathnameHost(),
					filespecAsPathname.pathnameDevice().eq(NILStruct.INSTANCE)
					? defaultPathspec.pathnameDevice() : filespecAsPathname.pathnameDevice(),
					filespecAsPathname.pathnameDirectory().eq(NILStruct.INSTANCE)
					? defaultPathspec.pathnameDirectory() : filespecAsPathname.pathnameDirectory(),
					filespecAsPathname.pathnameName().eq(NILStruct.INSTANCE)
					? defaultPathspec.pathnameName() : filespecAsPathname.pathnameName(),
					filespecAsPathname.pathnameType().eq(NILStruct.INSTANCE)
					? defaultPathspec.pathnameType() : filespecAsPathname.pathnameType(),
					filespecAsPathname.pathnameVersion().eq(NILStruct.INSTANCE)
					? defaultPathspec.pathnameVersion() : filespecAsPathname.pathnameVersion()
			);
//			filespecPathname = PathnameStruct.fromDesignator(filespec);
		}

		final File pathnameFile = new File(filespecPathname.namestring());

		if (!pathnameFile.exists() && ifDoesNotExist) {
			throw new FileErrorException("Filespec provided to LOAD does not exist: " + pathnameFile, filespecFileStream);
		}
		if (!pathnameFile.exists()) {
			return NILStruct.INSTANCE;
		}

		final LispStruct previousLoadPathname = CommonLispSymbols.LOAD_PATHNAME_VAR.symbolValue();
		final LispStruct previousLoadTruename = CommonLispSymbols.LOAD_TRUENAME_VAR.symbolValue();

		CommonLispSymbols.COMPILE_FILE_PATHNAME_VAR.setfSymbolValue(filespecPathname);
		final PathnameStruct filespecTruename = PathnameStruct.toPathname(pathnameFile.toURI().toString());
		CommonLispSymbols.COMPILE_FILE_TRUENAME_VAR.setfSymbolValue(filespecTruename);

		final ReadtableStruct previousReadtable = CommonLispSymbols.READTABLE_VAR.getVariableValue();
		final PackageStruct previousPackage = CommonLispSymbols.PACKAGE_VAR.getVariableValue();

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
			CommonLispSymbols.LOAD_TRUENAME_VAR.setfSymbolValue(previousLoadTruename);
			CommonLispSymbols.LOAD_PATHNAME_VAR.setfSymbolValue(previousLoadPathname);

			CommonLispSymbols.PACKAGE_VAR.setfSymbolValue(previousPackage);
			CommonLispSymbols.READTABLE_VAR.setfSymbolValue(previousReadtable);
		}
	}

	private static LispStruct loadSourceCode(final FileStreamStruct filespecFileStream, final File pathnameFile,
	                                         final boolean verbose, final boolean print) {

		if (verbose) {
			log.info("; Loading '{}'", pathnameFile);
		}

		final LispStruct eofValue = new LispStruct() {
		};

		LispStruct form;
		do {
			form = InternalRead.read(filespecFileStream, NILStruct.INSTANCE, eofValue, NILStruct.INSTANCE);
			if (form == eofValue) {
				continue;
			}

			final LispStruct evaluatedForm = InternalEval.eval(form);
			if (print) {
				log.info("; {}", evaluatedForm);
			}
		} while (form != eofValue);

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
