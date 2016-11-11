/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import jcl.lang.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.NILStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.statics.StreamVariables;
import org.springframework.stereotype.Component;

@Component
public final class ClearInputFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "CLEAR-INPUT";
	private static final String INPUT_STREAM_ARGUMENT = "INPUT-STREAM";

	public ClearInputFunction() {
		super("Clears any available input from input-stream.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .optionalParameter(INPUT_STREAM_ARGUMENT).withInitialValue(StreamVariables.STANDARD_INPUT.getVariableValue())
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final InputStreamStruct inputStreamStruct = arguments.getOptionalArgument(INPUT_STREAM_ARGUMENT, InputStreamStruct.class);
		inputStreamStruct.clearInput();
		return NILStruct.INSTANCE;
	}
}
