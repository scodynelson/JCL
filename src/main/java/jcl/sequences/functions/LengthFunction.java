/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.sequences.functions;

import java.math.BigInteger;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.numbers.IntegerStruct;
import jcl.sequences.SequenceStruct;
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
		return new IntegerStruct(BigInteger.valueOf(length));
	}
}
