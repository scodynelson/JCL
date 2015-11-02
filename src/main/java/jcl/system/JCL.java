package jcl.system;

import java.io.IOException;
import java.util.List;

import jcl.compiler.functions.CompileFileFunction;
import jcl.conditions.exceptions.ErrorException;
import jcl.pathnames.PathnameName;
import jcl.pathnames.PathnameStruct;
import jcl.pathnames.PathnameType;
import jcl.pathnames.functions.MergePathnamesFunction;
import jcl.streams.CharacterStreamStruct;
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
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ImportResource;

@Configuration
@ImportResource("applicationContext.xml")
@ComponentScan("jcl")
@SpringBootApplication
public class JCL implements ApplicationRunner {

	private static final Logger LOGGER = LoggerFactory.getLogger(JCL.class);

	@Autowired
	private CompileFileFunction compileFileFunction;

	@Autowired
	private ReadEvalPrint readEvalPrint;

	@Autowired
	private MergePathnamesFunction mergePathnamesFunction;

	public JCL() throws IOException {
		try (LoggerOutputStream loggerOutputStream = new LoggerOutputStream(LOGGER)) {
			final CharacterStreamStruct characterStream = new CharacterStreamStruct(System.in, loggerOutputStream);

			final TwoWayStreamStruct terminalIoStream = new TwoWayStreamStruct(characterStream, characterStream);
			StreamVariables.TERMINAL_IO.setValue(terminalIoStream);
		}
	}

	public static void main(final String... args) {
		SpringApplication.run(JCL.class, args).close();
	}

	@Override
	public void run(final ApplicationArguments args) throws Exception {

		final boolean compileFileSrcDir = args.containsOption("compileFileSrcDir");
		final boolean compileFileDestDir = args.containsOption("compileFileDestDir");
		if(compileFileSrcDir && compileFileDestDir) {
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
		} else if(compileFileSrcDir || compileFileDestDir) {
			throw new ErrorException("Both Compile File Source and Destination directories must be provided.");
		} else {
			readEvalPrint.funcall(args);
		}
	}
}
