/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.sequence;

import java.math.BigInteger;

import jcl.lang.LispStruct;
import jcl.lang.sequence.SequenceStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.number.IntegerStruct;
import org.springframework.stereotype.Component;

@Component
public final class LengthFunction extends CommonLispBuiltInFunctionStruct {

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
		return IntegerStruct.valueOf(BigInteger.valueOf(length));
	}
}
