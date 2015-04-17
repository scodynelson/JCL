package jcl.system;

import java.io.IOException;

import jcl.streams.CharacterStreamStruct;
import jcl.streams.StreamVariables;
import jcl.streams.TwoWayStreamStruct;
import jcl.system.repl.ReadEvalPrint;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.ImportResource;

@Configuration
@ImportResource("applicationContext.xml")
@ComponentScan("jcl")
public class JCL {

	private static final Logger LOGGER = LoggerFactory.getLogger(JCL.class);

	public JCL() throws IOException {
		try (LoggerOutputStream loggerOutputStream = new LoggerOutputStream(LOGGER)) {
			final CharacterStreamStruct characterStream = new CharacterStreamStruct(System.in, loggerOutputStream);

			final TwoWayStreamStruct terminalIoStream = new TwoWayStreamStruct(characterStream, characterStream);
			StreamVariables.TERMINAL_IO.setValue(terminalIoStream);
		}
	}

	public static void main(final String... args) {

		try (final AutoCloseable context = new AnnotationConfigApplicationContext(JCL.class)) {
			final ReadEvalPrint repl = ((ApplicationContext) context).getBean(ReadEvalPrint.class);

			repl.funcall(args);
		} catch (final BeansException e) {
			e.printStackTrace();
		} catch (final Exception e) {
			e.printStackTrace();
		}
		System.exit(0);
	}
}
