package jcl.system;

import jcl.compiler.old.CompilerClassLoader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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

	public static ClassLoader CURRENT_CLASSLOADER = CompilerClassLoader.Loader;

	public static void main(final String... args) {
		// make a new instance of JCL

		try (final AutoCloseable context = new AnnotationConfigApplicationContext(JCL.class)) {
			final ReadEvalPrint repl = ((ApplicationContext) context).getBean(ReadEvalPrint.class);

			final Object result = repl.funcall(args);
			LOGGER.debug("JCL returned => {}", result);
		} catch (Exception e) {
			e.printStackTrace();
		}
		System.exit(0);
	}
}
