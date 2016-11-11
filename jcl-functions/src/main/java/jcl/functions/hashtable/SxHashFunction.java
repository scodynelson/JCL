/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.hashtable;

import java.math.BigInteger;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.LispStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

@Component
public final class SxHashFunction extends CommonLispBuiltInFunctionStructBase {

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
		return LispStructFactory.toInteger(BigInteger.valueOf(hashCode));
	}
}
