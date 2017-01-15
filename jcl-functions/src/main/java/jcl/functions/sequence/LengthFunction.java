/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.sequence;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.LispStruct;
import jcl.lang.SequenceStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
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
		return sequence.length();
	}
}
