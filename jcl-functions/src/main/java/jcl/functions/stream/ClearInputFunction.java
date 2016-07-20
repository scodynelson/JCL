/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.stream.InputStream;
import jcl.lang.stream.StreamVariables;
import org.springframework.stereotype.Component;

@Component
public final class ClearInputFunction extends CommonLispBuiltInFunctionStruct {

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
		final InputStream inputStream = arguments.getOptionalArgument(INPUT_STREAM_ARGUMENT, InputStream.class);
		inputStream.clearInput();
		return NILStruct.INSTANCE;
	}
}