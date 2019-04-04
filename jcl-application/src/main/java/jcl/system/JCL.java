package jcl.system;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import jcl.compiler.function.CompileForm;
import jcl.functions.CompileFileFunction;
import jcl.functions.FuncallFunction;
import jcl.functions.LoadFunction;
import jcl.lang.FunctionStruct;
import jcl.lang.JavaStreamStruct;
import jcl.lang.PathnameStruct;
import jcl.lang.TwoWayStreamStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.pathname.PathnameName;
import jcl.lang.pathname.PathnameType;
import jcl.lang.statics.CompilerVariables;
import jcl.lang.statics.StreamVariables;
import jcl.system.repl.ReadEvalPrint;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ConfigurableApplicationContext;

@SpringBootApplication(scanBasePackages = "jcl")
public class JCL implements ApplicationRunner {

	private static final Logger LOGGER = LoggerFactory.getLogger(JCL.class);

	@Autowired
	private ConfigurableApplicationContext context;

	@Autowired
	private CompileFileFunction compileFileFunction;

	@Autowired
	private ReadEvalPrint readEvalPrint;

	@Autowired
	private LoadFunction loadFunction;

	@Autowired
	private FuncallFunction funcallFunction;

	private final List<String> lispFilesToLoad;

	public JCL() throws IOException {
		BootstrapSymbols.bootstrap();

		try (LoggerOutputStream loggerOutputStream = new LoggerOutputStream(LOGGER)) {
			final JavaStreamStruct characterStream = JavaStreamStruct.toJavaStream(System.in, loggerOutputStream);

			// TODO: Constructing stream directly from Impl
			final TwoWayStreamStruct terminalIoStream = TwoWayStreamStruct.toTwoWayStream(true, characterStream, characterStream);
			StreamVariables.TERMINAL_IO.setValue(terminalIoStream);
		}
		
		lispFilesToLoad = Arrays.asList(
				"jcl-application/src/main/lisp/jcl/compiler/base-macro-lambdas.lisp",
				"jcl-application/src/main/lisp/jcl/compiler/macros.lisp",
				"jcl-application/src/main/lisp/jcl/iterators/iterators.lisp",
				"jcl-application/src/main/lisp/jcl/characters/characters.lisp",
				"jcl-application/src/main/lisp/jcl/pathnames/pathnames.lisp",
				"jcl-application/src/main/lisp/jcl/reader/reader.lisp",
				"jcl-application/src/main/lisp/jcl/streams/streams.lisp",
				"jcl-application/src/main/lisp/jcl/symbols/symbols.lisp",
				"jcl-application/src/main/lisp/jcl/packages/packages.lisp",
				"jcl-application/src/main/lisp/jcl/lists/lists.lisp",
				"jcl-application/src/main/lisp/jcl/numbers/numbers.lisp",
				"jcl-application/src/main/lisp/jcl/hashtables/hashtables.lisp",
				"jcl-application/src/main/lisp/jcl/strings/strings.lisp"
		);
	}

	public static void main(final String... args) {
		try (final ConfigurableApplicationContext context = SpringApplication.run(JCL.class, args)) {
			context.registerShutdownHook();
		}
	}

	@Override
	public void run(final ApplicationArguments args) throws Exception {
		initializeLispFunctions();

		CompilerVariables.MACROEXPAND_HOOK.setValue(funcallFunction);

		final boolean compileFileSrcDir = args.containsOption("compileFileSrcDir");
		final boolean compileFileDestDir = args.containsOption("compileFileDestDir");
		if (compileFileSrcDir && compileFileDestDir) {
			compileSourceFiles(args);
		} else if (compileFileSrcDir || compileFileDestDir) {
			throw new ErrorException("Both Compile File Source and Destination directories must be provided.");
		} else {
			loadLispFiles();
			readEvalPrint.funcall(args);
		}
	}

	private void initializeLispFunctions() throws Exception {
		final Map<String, FunctionStruct> functionBeans = context.getBeansOfType(FunctionStruct.class);
		for (final FunctionStruct function : functionBeans.values()) {
			function.afterPropertiesSet();
		}
	}

	private void compileSourceFiles(final ApplicationArguments args) {
		final List<String> sourceFiles = args.getOptionValues("compileFileSrcDir");
		final String destDir = args.getOptionValues("compileFileDestDir").get(0);

		for (final String fileName : sourceFiles) {
			final PathnameStruct sourceFile = PathnameStruct.toPathname(fileName);

			final PathnameName pathnameName = sourceFile.getPathnameName();
			final PathnameType pathnameType = new PathnameType("jar");
			final PathnameStruct tempPathname = PathnameStruct.toPathname(null, null, null, pathnameName, pathnameType, null);

			final PathnameStruct destDirectory = PathnameStruct.toPathname(destDir);
			final PathnameStruct newSourceFile = PathnameStruct.mergePathnames(destDirectory, tempPathname);

			compileFileFunction.compileFile(sourceFile, newSourceFile, true, true);
		}
	}

	private void loadLispFiles() {
//		for (final String lispFileToLoad : lispFilesToLoad) {
//			final PathnameStruct pathname = PathnameStruct.toPathname(lispFileToLoad);
//			loadFunction.load(pathname, false, false, true);
//		}
		CompileForm.OUTPUT_FILE = false;
		PathnameStruct pathname = PathnameStruct.toPathname("jcl-application/src/main/lisp/jcl/compiler/base-macro-lambdas.lisp");
		loadFunction.load(pathname, false, false, true);
		CompileForm.OUTPUT_FILE = true;

		CompileForm.OUTPUT_FILE = false;
		pathname = PathnameStruct.toPathname("jcl-application/src/main/lisp/jcl/compiler/macros.lisp");
		loadFunction.load(pathname, false, false, true);
		CompileForm.OUTPUT_FILE = true;

		CompileForm.OUTPUT_FILE = false;
		pathname = PathnameStruct.toPathname("jcl-application/src/main/lisp/jcl/iterators/iterators.lisp");
		loadFunction.load(pathname, false, false, true);
		CompileForm.OUTPUT_FILE = true;

		CompileForm.OUTPUT_FILE = false;
		pathname = PathnameStruct.toPathname("jcl-application/src/main/lisp/jcl/characters/characters.lisp");
		loadFunction.load(pathname, false, false, true);
		CompileForm.OUTPUT_FILE = true;

		CompileForm.OUTPUT_FILE = false;
		pathname = PathnameStruct.toPathname("jcl-application/src/main/lisp/jcl/pathnames/pathnames.lisp");
		loadFunction.load(pathname, false, false, true);
		CompileForm.OUTPUT_FILE = true;

		CompileForm.OUTPUT_FILE = false;
		pathname = PathnameStruct.toPathname("jcl-application/src/main/lisp/jcl/reader/reader.lisp");
		loadFunction.load(pathname, false, false, true);
		CompileForm.OUTPUT_FILE = true;

		CompileForm.OUTPUT_FILE = false;
		pathname = PathnameStruct.toPathname("jcl-application/src/main/lisp/jcl/streams/streams.lisp");
		loadFunction.load(pathname, false, false, true);
		CompileForm.OUTPUT_FILE = true;

		CompileForm.OUTPUT_FILE = false;
		pathname = PathnameStruct.toPathname("jcl-application/src/main/lisp/jcl/symbols/symbols.lisp");
		loadFunction.load(pathname, false, false, true);
		CompileForm.OUTPUT_FILE = true;

		CompileForm.OUTPUT_FILE = false;
		pathname = PathnameStruct.toPathname("jcl-application/src/main/lisp/jcl/packages/packages.lisp");
		loadFunction.load(pathname, false, false, true);
		CompileForm.OUTPUT_FILE = true;

		CompileForm.OUTPUT_FILE = false;
		pathname = PathnameStruct.toPathname("jcl-application/src/main/lisp/jcl/lists/lists.lisp");
		loadFunction.load(pathname, false, false, true);
		CompileForm.OUTPUT_FILE = true;

		CompileForm.OUTPUT_FILE = false;
		pathname = PathnameStruct.toPathname("jcl-application/src/main/lisp/jcl/numbers/numbers.lisp");
		loadFunction.load(pathname, false, false, true);
		CompileForm.OUTPUT_FILE = true;

		CompileForm.OUTPUT_FILE = false;
		pathname = PathnameStruct.toPathname("jcl-application/src/main/lisp/jcl/hashtables/hashtables.lisp");
		loadFunction.load(pathname, false, false, true);
		CompileForm.OUTPUT_FILE = true;

		CompileForm.OUTPUT_FILE = false;
		pathname = PathnameStruct.toPathname("jcl-application/src/main/lisp/jcl/strings/strings.lisp");
		loadFunction.load(pathname, false, false, true);
		CompileForm.OUTPUT_FILE = true;
	}
}
