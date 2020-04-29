package jcl.system;

import java.util.List;

import jcl.compiler.function.InternalCompile;
import jcl.compiler.function.InternalLoad;
import jcl.compiler.sa.BootstrapExpanders;
import jcl.lang.CharacterStreamStruct;
import jcl.lang.PathnameStruct;
import jcl.lang.TStruct;
import jcl.lang.TwoWayStreamStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.pathname.PathnameName;
import jcl.lang.pathname.PathnameType;
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.statics.CompilerVariables;
import jcl.system.repl.ReadEvalPrint;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.collections4.CollectionUtils;
import picocli.CommandLine;
import picocli.CommandLine.Command;
import picocli.CommandLine.Option;

@Log4j2
@Command(name = "jcl", mixinStandardHelpOptions = true)
public class JCL implements Runnable {

	@Option(names = "--compileFileSrcDir")
	private List<String> compileFileSrcDir;

	@Option(names = "--compileFileDestDir")
	private List<String> compileFileDestDir;

	public JCL() throws Exception {
		BootstrapSymbols.bootstrapStreams();

		try (final LoggerOutputStream loggerOutputStream = new LoggerOutputStream(log)) {
			final CharacterStreamStruct characterStream = CharacterStreamStruct.toCharacterStream(System.in, loggerOutputStream);

			final TwoWayStreamStruct terminalIoStream = TwoWayStreamStruct.toTwoWayStream(characterStream, characterStream);
			terminalIoStream.setInteractive(true);
			CommonLispSymbols.TERMINAL_IO.setValue(terminalIoStream);
		}
	}

	public static void main(final String... args) throws Exception {
		new CommandLine(new JCL()).execute(args);
	}

	@Override
	public void run() {
		BootstrapSymbols.bootstrap();
		BootstrapFunctions.bootstrap();
		BootstrapExpanders.bootstrap();

		final boolean hasCompileFileSrcDir = CollectionUtils.isNotEmpty(compileFileSrcDir);
		final boolean hasCompileFileDestDir = CollectionUtils.isNotEmpty(compileFileDestDir);
		if (hasCompileFileSrcDir && hasCompileFileDestDir) {
			compileSourceFiles();
		} else if (hasCompileFileSrcDir || hasCompileFileDestDir) {
			throw new ErrorException("Both Compile File Source and Destination directories must be provided.");
		} else {
//			CompilerVariables.LOAD_VERBOSE.setValue(TStruct.INSTANCE);

			InternalLoad.autoLoadJavaModules();
			ReadEvalPrint.funcall();
		}
	}

	private void compileSourceFiles() {
		final List<String> sourceFiles = compileFileSrcDir;
		final String destDir = compileFileDestDir.get(0);

		CompilerVariables.COMPILE_VERBOSE.setValue(TStruct.INSTANCE);
		CompilerVariables.LOAD_VERBOSE.setValue(TStruct.INSTANCE);

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
}
