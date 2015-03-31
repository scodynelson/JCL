/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.icg.generator.testground;

import jcl.LispStruct;
import jcl.functions.Closure;
import jcl.functions.FunctionStruct;

public class TestGroundLambdaFunction extends FunctionStruct {

	private static final long serialVersionUID = -1939696402314939143L;

	public TestGroundLambdaFunction(final Closure parentClosure) {
		closure = parentClosure;
	}

	@Override
	public LispStruct apply(final LispStruct... lispStructs) {
		return null;
	}
}
