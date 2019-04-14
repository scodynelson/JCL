/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.system.function;

import jcl.functions.ExtensionsBuiltInFunctionStructBase;
import jcl.lang.LispStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.boot.SpringApplication;
import org.springframework.context.ApplicationContext;

public final class QuitFunction extends ExtensionsBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "QUIT";

	private final ApplicationContext context;

	public QuitFunction(final ApplicationContext context) {
		super("Quits the JCL Application.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		);
		this.context = context;
	}

	@SuppressWarnings("all")
	@Override
	public LispStruct apply(final Arguments arguments) {
		System.exit(SpringApplication.exit(context));
		return null;
	}
}
