package jcl;

import jcl.system.ReadEvalPrint;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public final class JCL {

	private static final Logger LOGGER = LoggerFactory.getLogger(JCL.class);

	private JCL() {
	}

	public static void main(final String... args) {
		// make a new instance of CLforJava
		final Object result = ReadEvalPrint.funcall(args);
		LOGGER.debug("\nJCL returned => {}", result);
		System.exit(0);
	}
}
