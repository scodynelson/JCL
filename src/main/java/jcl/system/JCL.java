package jcl.system;

import java.io.IOException;

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
	private ReadEvalPrint readEvalPrint;

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
		readEvalPrint.funcall(args);
	}
}
