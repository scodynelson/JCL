package jcl.system;

import jcl.system.repl.ReadEvalPrint;
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
