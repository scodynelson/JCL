/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package testground;

import jcl.compiler.environment.Environment;
import jcl.compiler.function.CompiledFunctionStruct;
import jcl.lang.LispStruct;

@SuppressWarnings("all")
public class TestGroundLambdaFunction extends CompiledFunctionStruct {

	public TestGroundLambdaFunction(final Environment environment) {
		super(environment);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		return null;
	}
}
