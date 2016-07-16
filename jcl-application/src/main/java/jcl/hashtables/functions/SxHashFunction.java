/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.hashtables.functions;

import java.math.BigInteger;

import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.number.IntegerStruct;
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
		return IntegerStruct.valueOf(BigInteger.valueOf(hashCode));
	}
}
