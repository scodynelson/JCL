/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.hashtables.functions;

import java.math.BigInteger;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.numbers.IntegerStruct;
import org.springframework.stereotype.Component;

@Component
public final class SxHashFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "SXHASH";
	private static final String OBJECT_ARGUMENT = "OBJECT";

	public SxHashFunction() {
		super("Returns a hash code for object.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(OBJECT_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct object = arguments.getRequiredArgument(OBJECT_ARGUMENT);
		final int hashCode = object.hashCode();
		return new IntegerStruct(BigInteger.valueOf(hashCode));
	}
}
