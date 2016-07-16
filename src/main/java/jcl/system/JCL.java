package jcl.system;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import jcl.compiler.CompilerVariables;
import jcl.compiler.functions.CompileFileFunction;
import jcl.compiler.functions.CompileForm;
import jcl.compiler.functions.LoadFunction;
import jcl.conditions.exceptions.ErrorException;
import jcl.functions.functions.FuncallFunction;
import jcl.pathnames.PathnameName;
import jcl.pathnames.PathnameStruct;
import jcl.pathnames.PathnameType;
import jcl.pathnames.functions.MergePathnamesFunction;
import jcl.streams.JavaStreamStruct;
import jcl.streams.StreamVariables;
import jcl.streams.TwoWayStreamStruct;
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
	private CompileFileFunction compileFileFunction;

	@Autowired
	private ReadEvalPrint readEvalPrint;

	@Autowired
	private MergePathnamesFunction mergePathnamesFunction;

	@Autowired
	private LoadFunction loadFunction;

	@Autowired
	private FuncallFunction funcallFunction;

	private final List<String> lispFilesToLoad;

	public JCL() throws IOException {
		try (LoggerOutputStream loggerOutputStream = new LoggerOutputStream(LOGGER)) {
			final JavaStreamStruct characterStream = new JavaStreamStruct(System.in, loggerOutputStream);

			final TwoWayStreamStruct terminalIoStream = new TwoWayStreamStruct(true, characterStream, characterStream);
			StreamVariables.TERMINAL_IO.setValue(terminalIoStream);
		}
		
		lispFilesToLoad = Arrays.asList(
				"src/main/lisp/jcl/compiler/base-macro-lambdas.lisp",
				"src/main/lisp/jcl/compiler/macros.lisp",
				"src/main/lisp/jcl/iterators/iterators.lisp",
				"src/main/lisp/jcl/lists/lists.lisp"
		);
	}

	public static void main(final String... args) {
		try (final ConfigurableApplicationContext context = SpringApplication.run(JCL.class, args)) {
			context.registerShutdownHook();
		}
	}

	@Override
	public void run(final ApplicationArguments args) throws Exception {
		CompilerVariables.MACROEXPAND_HOOK.setValue(funcallFunction);
		loadLispFiles();

		final boolean compileFileSrcDir = args.containsOption("compileFileSrcDir");
		final boolean compileFileDestDir = args.containsOption("compileFileDestDir");
		if (compileFileSrcDir && compileFileDestDir) {
			compileSourceFiles(args);
		} else if (compileFileSrcDir || compileFileDestDir) {
			throw new ErrorException("Both Compile File Source and Destination directories must be provided.");
		} else {
			readEvalPrint.funcall(args);
		}
	}

	private void compileSourceFiles(final ApplicationArguments args) {
		final List<String> sourceFiles = args.getOptionValues("compileFileSrcDir");
		final String destDir = args.getOptionValues("compileFileDestDir").get(0);

		for (final String fileName : sourceFiles) {
			final PathnameStruct sourceFile = new PathnameStruct(fileName);

			final PathnameName pathnameName = sourceFile.getPathnameName();
			final PathnameType pathnameType = new PathnameType("jar");
			final PathnameStruct tempPathname = new PathnameStruct(null, null, null, pathnameName, pathnameType, null);

			final PathnameStruct destDirectory = new PathnameStruct(destDir);
			final PathnameStruct newSourceFile = mergePathnamesFunction.mergePathnames(destDirectory, tempPathname);

			compileFileFunction.compileFile(sourceFile, newSourceFile, true, true);
		}
	}

	private void loadLispFiles() {
		CompileForm.OUTPUT_FILE = false;
		for (final String lispFileToLoad : lispFilesToLoad) {
			final PathnameStruct pathname = new PathnameStruct(lispFileToLoad);
			loadFunction.load(pathname, false, false, true);
		}
		CompileForm.OUTPUT_FILE = true;
	}
}
