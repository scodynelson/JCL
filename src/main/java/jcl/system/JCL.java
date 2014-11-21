package jcl.system;

import jcl.compiler.old.CompilerClassLoader;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class JCL {

	private static final Logger LOGGER = LoggerFactory.getLogger(JCL.class);

	public static ClassLoader CURRENT_CLASSLOADER = CompilerClassLoader.Loader;

	private JCL() {
	}

	public static void main(final String... args) {
		// make a new instance of CLforJava
		final Object result = ReadEvalPrint.funcall(args);
		LOGGER.debug("\nJCL returned => {}", result);
		System.exit(0);
	}
}
