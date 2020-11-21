package jcl.system;

import java.util.List;

import jcl.compiler.function.InternalCompile;
import jcl.compiler.function.InternalLoad;
import jcl.compiler.sa.BootstrapExpanders;
import jcl.lang.BooleanStruct;
import jcl.lang.CharacterStreamStruct;
import jcl.lang.ConsStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.PathnameStruct;
import jcl.lang.StringStruct;
import jcl.lang.TStruct;
import jcl.lang.TwoWayStreamStruct;
import jcl.lang.ValuesStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.statics.CompilerVariables;
import jcl.system.repl.ReadEvalPrint;
import lombok.extern.log4j.Log4j2;
import org.apache.commons.collections4.CollectionUtils;
import picocli.CommandLine;

@Log4j2
@CommandLine.Command(name = "jcl", mixinStandardHelpOptions = true)
public class JCL implements Runnable {

	@CommandLine.Option(names = "--compileFileSrcDir")
	private List<String> compileFileSrcDir;

	@CommandLine.Option(names = "--compileFileDestDir")
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
		System.exit(new CommandLine(new JCL()).execute(args));
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

			final LispStruct pathnameName = sourceFile.pathnameName();
			final LispStruct pathnameType = StringStruct.toLispString("jar");

			final PathnameStruct destDirectory = PathnameStruct.toPathname(destDir);
			final PathnameStruct newSourceFile = mergePathnames(destDirectory, pathnameName, pathnameType);

			final LispStruct result = InternalCompile.compileFile(sourceFile, newSourceFile, TStruct.INSTANCE, TStruct.INSTANCE, null);
			final BooleanStruct failureP = (BooleanStruct) ((ValuesStruct) result).getValuesList().get(2);
			if (failureP.toJavaPBoolean()) {
				throw new ProgramErrorException("Failed to compile source file: " + sourceFile);
			}
		}
	}

	private static PathnameStruct mergePathnames(final PathnameStruct pathname, final LispStruct pathnameName, final LispStruct pathnameType) {
		LispStruct resultHost = NILStruct.INSTANCE;
		final LispStruct pathnameHost = pathname.pathnameHost();
		if (pathnameHost != NILStruct.INSTANCE) {
			resultHost = pathnameHost;
		}

		LispStruct resultDevice = NILStruct.INSTANCE;
		final LispStruct pathnameDevice = pathname.pathnameDevice();
		if (pathnameDevice != NILStruct.INSTANCE) {
			resultDevice = pathnameDevice;
		}

		LispStruct resultDirectory = NILStruct.INSTANCE;
		if (pathnameDevice instanceof ConsStruct) {
			final ConsStruct jars = (ConsStruct) resultDevice;
			final LispStruct jar = jars.car();
			if (jar instanceof PathnameStruct) {
				PathnameStruct o = mergePathnames((PathnameStruct)jar, pathnameName, pathnameType);
				if ((o.pathnameDirectory() instanceof ConsStruct)
						&& ((ConsStruct) o.pathnameDirectory()).length().eql(IntegerStruct.ONE)) { // i.e. (:ABSOLUTE) or (:RELATIVE)
					o = PathnameStruct.toPathname(o.pathnameHost(), o.pathnameDevice(), NILStruct.INSTANCE, o.pathnameName(), o.pathnameType(), o.pathnameVersion());
				}
				((ConsStruct)resultDevice).rplaca(o);
			}
			resultDirectory = pathname.pathnameDirectory();
		} else {
			if (pathname.pathnameDirectory() != NILStruct.INSTANCE) {
				final ListStruct dir = (ListStruct) pathname.pathnameDirectory();
				dir.car();
				resultDirectory = dir;
			}
		}

		final LispStruct resultName;
		if (pathname.pathnameName() == NILStruct.INSTANCE) {
			resultName = pathnameName;
		} else {
			resultName = pathname.pathnameName();
		}

		final LispStruct resultType;
		if (pathname.pathnameType() == NILStruct.INSTANCE) {
			resultType = pathnameType;
		} else {
			resultType = pathname.pathnameType();
		}

		LispStruct resultVersion = CommonLispSymbols.NEWEST_KEYWORD;
		if (pathname.pathnameVersion() != NILStruct.INSTANCE) {
			resultVersion = pathname.pathnameVersion();
		}

		return PathnameStruct.toPathname(resultHost, resultDevice, resultDirectory, resultName, resultType, resultVersion);
	}

}
