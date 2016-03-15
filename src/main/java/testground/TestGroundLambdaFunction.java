/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package testground;

import jcl.LispStruct;
import jcl.functions.Closure;
import jcl.functions.CompiledFunctionStruct;

@SuppressWarnings("all")
public class TestGroundLambdaFunction extends CompiledFunctionStruct {

	public TestGroundLambdaFunction(final Closure parentClosure) {
		closure = parentClosure;
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		return null;
	}
}
