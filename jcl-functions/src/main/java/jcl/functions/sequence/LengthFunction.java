/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.sequence;

import java.math.BigInteger;

import jcl.lang.LispStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.CommonLispBuiltInFunctionStructBase;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.SequenceStruct;
import org.springframework.stereotype.Component;

@Component
public final class LengthFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "LENGTH";
	private static final String SEQUENCE_ARGUMENT = "SEQUENCE";

	public LengthFunction() {
		super("Returns the number of elements in sequence",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(SEQUENCE_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final SequenceStruct sequence = arguments.getRequiredArgument(SEQUENCE_ARGUMENT, SequenceStruct.class);
		final Long length = sequence.length();
		return LispStructFactory.toInteger(BigInteger.valueOf(length));
	}
}
