package jcl.system;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import jcl.compiler.functions.CompileForm;
import jcl.functions.CompileFileFunction;
import jcl.functions.FuncallFunction;
import jcl.functions.LoadFunction;
import jcl.functions.pathname.MergePathnamesFunction;
import jcl.lang.CompilerVariables;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.pathname.PathnameName;
import jcl.lang.pathname.PathnameStruct;
import jcl.lang.pathname.PathnameType;
import jcl.lang.stream.JavaStreamStructImpl;
import jcl.lang.stream.StreamVariables;
import jcl.lang.stream.TwoWayStreamStructImpl;
import jcl.system.repl.ReadEvalPrint;
import jcl.util.SimpleThreadScope;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.CustomScopeConfigurer;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ConfigurableApplicationContext;
import org.springframework.context.annotation.Bean;

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
			final JavaStreamStructImpl characterStream = LispStructFactory.toJavaStream(System.in, loggerOutputStream);

			final TwoWayStreamStructImpl terminalIoStream = LispStructFactory.toTwoWayStream(true, characterStream, characterStream);
			StreamVariables.TERMINAL_IO.setValue(terminalIoStream);
		}
		
		lispFilesToLoad = Arrays.asList(
				"jcl-application/src/main/lisp/jcl/compiler/base-macro-lambdas.lisp",
				"jcl-application/src/main/lisp/jcl/compiler/macros.lisp",
				"jcl-application/src/main/lisp/jcl/iterators/iterators.lisp",
				"jcl-application/src/main/lisp/jcl/lists/lists.lisp"
		);
	}

	public static void main(final String... args) {
		try (final ConfigurableApplicationContext context = SpringApplication.run(JCL.class, args)) {
			context.registerShutdownHook();
		}
	}

	@Bean
	public static CustomScopeConfigurer threadScopeConfigurer() {
		final CustomScopeConfigurer scopeConfigurer = new CustomScopeConfigurer();
		scopeConfigurer.addScope("thread", new SimpleThreadScope());
		return scopeConfigurer;
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
			final PathnameStruct sourceFile = LispStructFactory.toPathname(fileName);

			final PathnameName pathnameName = sourceFile.getPathnameName();
			final PathnameType pathnameType = new PathnameType("jar");
			final PathnameStruct tempPathname = LispStructFactory.toPathname(null, null, null, pathnameName, pathnameType, null);

			final PathnameStruct destDirectory = LispStructFactory.toPathname(destDir);
			final PathnameStruct newSourceFile = mergePathnamesFunction.mergePathnames(destDirectory, tempPathname);

			compileFileFunction.compileFile(sourceFile, newSourceFile, true, true);
		}
	}

	private void loadLispFiles() {
		CompileForm.OUTPUT_FILE = false;
		for (final String lispFileToLoad : lispFilesToLoad) {
			final PathnameStruct pathname = LispStructFactory.toPathname(lispFileToLoad);
			loadFunction.load(pathname, false, false, true);
		}
		CompileForm.OUTPUT_FILE = true;
	}
}
