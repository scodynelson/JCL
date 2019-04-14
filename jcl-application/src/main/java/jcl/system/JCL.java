package jcl.system;

import java.util.Arrays;
import java.util.List;

import jcl.compiler.function.CompileForm;
import jcl.compiler.function.InternalCompile;
import jcl.compiler.function.InternalLoad;
import jcl.compiler.sa.BootstrapExpanders;
import jcl.lang.JavaStreamStruct;
import jcl.lang.NILStruct;
import jcl.lang.PathnameStruct;
import jcl.lang.TStruct;
import jcl.lang.TwoWayStreamStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.pathname.PathnameName;
import jcl.lang.pathname.PathnameType;
import jcl.lang.statics.StreamVariables;
import jcl.system.repl.ReadEvalPrint;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.ApplicationArguments;
import org.springframework.boot.ApplicationRunner;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.builder.SpringApplicationBuilder;
import org.springframework.context.ConfigurableApplicationContext;

@Slf4j
@SpringBootApplication
public class JCL implements ApplicationRunner {

	@Autowired
	private ConfigurableApplicationContext context;

	private final List<String> lispFilesToLoad;

	public JCL() throws Exception {
		try (LoggerOutputStream loggerOutputStream = new LoggerOutputStream(log)) {
			final JavaStreamStruct characterStream = JavaStreamStruct.toJavaStream(System.in, loggerOutputStream);

			// TODO: Constructing stream directly from Impl
			final TwoWayStreamStruct terminalIoStream = TwoWayStreamStruct.toTwoWayStream(true, characterStream, characterStream);
			StreamVariables.TERMINAL_IO.setValue(terminalIoStream);
		}

		lispFilesToLoad = Arrays.asList(
				"jcl-application/src/main/lisp/jcl/compiler/base-macro-lambdas.lisp",
				"jcl-application/src/main/lisp/jcl/sequences/sequences.lisp",
				"jcl-application/src/main/lisp/jcl/lists/base-lists.lisp",
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
				"jcl-application/src/main/lisp/jcl/strings/strings.lisp",
				"jcl-application/src/main/lisp/jcl/environment/environment.lisp",
				"jcl-application/src/main/lisp/jcl/structures/structures.lisp"
		);
	}

	public static void main(final String... args) {
		final SpringApplicationBuilder applicationBuilder = new SpringApplicationBuilder(JCL.class);
		applicationBuilder.headless(false);

		try (final ConfigurableApplicationContext context = applicationBuilder.run(args)) {
			context.registerShutdownHook();
		}
	}

	@Override
	public void run(final ApplicationArguments args) throws Exception {
		BootstrapSymbols.bootstrap();
		BootstrapFunctions.bootstrap(context);
		BootstrapExpanders.bootstrap();

		final boolean compileFileSrcDir = args.containsOption("compileFileSrcDir");
		final boolean compileFileDestDir = args.containsOption("compileFileDestDir");
		if (compileFileSrcDir && compileFileDestDir) {
			compileSourceFiles(args);
		} else if (compileFileSrcDir || compileFileDestDir) {
			throw new ErrorException("Both Compile File Source and Destination directories must be provided.");
		} else {
			loadLispFiles();
			ReadEvalPrint.funcall(args);
		}
	}

	private static void compileSourceFiles(final ApplicationArguments args) {
		final List<String> sourceFiles = args.getOptionValues("compileFileSrcDir");
		final String destDir = args.getOptionValues("compileFileDestDir").get(0);

		for (final String fileName : sourceFiles) {
			final PathnameStruct sourceFile = PathnameStruct.toPathname(fileName);

			final PathnameName pathnameName = sourceFile.getPathnameName();
			final PathnameType pathnameType = new PathnameType("jar");
			final PathnameStruct tempPathname = PathnameStruct.toPathname(null, null, null, pathnameName, pathnameType, null);

			final PathnameStruct destDirectory = PathnameStruct.toPathname(destDir);
			final PathnameStruct newSourceFile = PathnameStruct.mergePathnames(destDirectory, tempPathname);

			InternalCompile.compileFile(sourceFile, newSourceFile, TStruct.INSTANCE, TStruct.INSTANCE, null);
		}
	}

	private static void loadLispFiles() {
//		for (final String lispFileToLoad : lispFilesToLoad) {
//			final PathnameStruct pathname = PathnameStruct.toPathname(lispFileToLoad);
//			InternalLoad.load(pathname, NILStruct.INSTANCE, NILStruct.INSTANCE, TStruct.INSTANCE, null);
//		}
		CompileForm.OUTPUT_FILE = false;
		PathnameStruct pathname = PathnameStruct.toPathname("jcl-application/src/main/lisp/jcl/compiler/base-macro-lambdas.lisp");
		InternalLoad.load(pathname, NILStruct.INSTANCE, NILStruct.INSTANCE, TStruct.INSTANCE, null);
		CompileForm.OUTPUT_FILE = true;

		CompileForm.OUTPUT_FILE = false;
		pathname = PathnameStruct.toPathname("jcl-application/src/main/lisp/jcl/sequences/sequences.lisp");
		InternalLoad.load(pathname, NILStruct.INSTANCE, NILStruct.INSTANCE, TStruct.INSTANCE, null);
		CompileForm.OUTPUT_FILE = true;

		CompileForm.OUTPUT_FILE = false;
		pathname = PathnameStruct.toPathname("jcl-application/src/main/lisp/jcl/lists/base-lists.lisp");
		InternalLoad.load(pathname, NILStruct.INSTANCE, NILStruct.INSTANCE, TStruct.INSTANCE, null);
		CompileForm.OUTPUT_FILE = true;

		CompileForm.OUTPUT_FILE = false;
		pathname = PathnameStruct.toPathname("jcl-application/src/main/lisp/jcl/compiler/macros.lisp");
		InternalLoad.load(pathname, NILStruct.INSTANCE, NILStruct.INSTANCE, TStruct.INSTANCE, null);
		CompileForm.OUTPUT_FILE = true;

		CompileForm.OUTPUT_FILE = false;
		pathname = PathnameStruct.toPathname("jcl-application/src/main/lisp/jcl/iterators/iterators.lisp");
		InternalLoad.load(pathname, NILStruct.INSTANCE, NILStruct.INSTANCE, TStruct.INSTANCE, null);
		CompileForm.OUTPUT_FILE = true;

		CompileForm.OUTPUT_FILE = false;
		pathname = PathnameStruct.toPathname("jcl-application/src/main/lisp/jcl/characters/characters.lisp");
		InternalLoad.load(pathname, NILStruct.INSTANCE, NILStruct.INSTANCE, TStruct.INSTANCE, null);
		CompileForm.OUTPUT_FILE = true;

		CompileForm.OUTPUT_FILE = false;
		pathname = PathnameStruct.toPathname("jcl-application/src/main/lisp/jcl/pathnames/pathnames.lisp");
		InternalLoad.load(pathname, NILStruct.INSTANCE, NILStruct.INSTANCE, TStruct.INSTANCE, null);
		CompileForm.OUTPUT_FILE = true;

		CompileForm.OUTPUT_FILE = false;
		pathname = PathnameStruct.toPathname("jcl-application/src/main/lisp/jcl/reader/reader.lisp");
		InternalLoad.load(pathname, NILStruct.INSTANCE, NILStruct.INSTANCE, TStruct.INSTANCE, null);
		CompileForm.OUTPUT_FILE = true;

		CompileForm.OUTPUT_FILE = false;
		pathname = PathnameStruct.toPathname("jcl-application/src/main/lisp/jcl/streams/streams.lisp");
		InternalLoad.load(pathname, NILStruct.INSTANCE, NILStruct.INSTANCE, TStruct.INSTANCE, null);
		CompileForm.OUTPUT_FILE = true;

		CompileForm.OUTPUT_FILE = false;
		pathname = PathnameStruct.toPathname("jcl-application/src/main/lisp/jcl/symbols/symbols.lisp");
		InternalLoad.load(pathname, NILStruct.INSTANCE, NILStruct.INSTANCE, TStruct.INSTANCE, null);
		CompileForm.OUTPUT_FILE = true;

		CompileForm.OUTPUT_FILE = false;
		pathname = PathnameStruct.toPathname("jcl-application/src/main/lisp/jcl/packages/packages.lisp");
		InternalLoad.load(pathname, NILStruct.INSTANCE, NILStruct.INSTANCE, TStruct.INSTANCE, null);
		CompileForm.OUTPUT_FILE = true;

		CompileForm.OUTPUT_FILE = false;
		pathname = PathnameStruct.toPathname("jcl-application/src/main/lisp/jcl/lists/lists.lisp");
		InternalLoad.load(pathname, NILStruct.INSTANCE, NILStruct.INSTANCE, TStruct.INSTANCE, null);
		CompileForm.OUTPUT_FILE = true;

		CompileForm.OUTPUT_FILE = false;
		pathname = PathnameStruct.toPathname("jcl-application/src/main/lisp/jcl/numbers/numbers.lisp");
		InternalLoad.load(pathname, NILStruct.INSTANCE, NILStruct.INSTANCE, TStruct.INSTANCE, null);
		CompileForm.OUTPUT_FILE = true;

		CompileForm.OUTPUT_FILE = false;
		pathname = PathnameStruct.toPathname("jcl-application/src/main/lisp/jcl/hashtables/hashtables.lisp");
		InternalLoad.load(pathname, NILStruct.INSTANCE, NILStruct.INSTANCE, TStruct.INSTANCE, null);
		CompileForm.OUTPUT_FILE = true;

		CompileForm.OUTPUT_FILE = false;
		pathname = PathnameStruct.toPathname("jcl-application/src/main/lisp/jcl/strings/strings.lisp");
		InternalLoad.load(pathname, NILStruct.INSTANCE, NILStruct.INSTANCE, TStruct.INSTANCE, null);
		CompileForm.OUTPUT_FILE = true;

		CompileForm.OUTPUT_FILE = false;
		pathname = PathnameStruct.toPathname("jcl-application/src/main/lisp/jcl/environment/environment.lisp");
		InternalLoad.load(pathname, NILStruct.INSTANCE, NILStruct.INSTANCE, TStruct.INSTANCE, null);
		CompileForm.OUTPUT_FILE = true;

		CompileForm.OUTPUT_FILE = false;
		pathname = PathnameStruct.toPathname("jcl-application/src/main/lisp/jcl/structures/structures.lisp");
		InternalLoad.load(pathname, NILStruct.INSTANCE, NILStruct.INSTANCE, TStruct.INSTANCE, null);
		CompileForm.OUTPUT_FILE = true;
	}
}
