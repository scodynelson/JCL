/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package testground;

import jcl.lang.LispStruct;
import jcl.compiler.function.Closure;
import jcl.compiler.function.CompiledFunctionStruct;

@SuppressWarnings("all")
public class TestGroundLambdaFunction extends CompiledFunctionStruct {

	public TestGroundLambdaFunction(final Closure parentClosure) {
		super(parentClosure);
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		return null;
	}
}