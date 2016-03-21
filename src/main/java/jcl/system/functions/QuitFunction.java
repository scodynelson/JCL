/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.system.functions;

import jcl.LispStruct;
import jcl.functions.ExtensionsBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.SpringApplication;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;

@Component
public final class QuitFunction extends ExtensionsBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "QUIT";

	@Autowired
	private ApplicationContext context;

	public QuitFunction() {
		super("Quits the JCL Application.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		);
	}

	@SuppressWarnings("all")
	@Override
	public LispStruct apply(final Arguments arguments) {
		System.exit(SpringApplication.exit(context));
		return null;
	}
}
