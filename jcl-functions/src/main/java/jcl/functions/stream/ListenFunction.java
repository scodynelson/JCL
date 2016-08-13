/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import jcl.lang.LispStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.CommonLispBuiltInFunctionStructBase;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.InputStreamStruct;
import jcl.lang.statics.StreamVariables;
import org.springframework.stereotype.Component;

@Component
public final class ListenFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "LISTEN";
	private static final String INPUT_STREAM_ARGUMENT = "INPUT-STREAM";

	public ListenFunction() {
		super("Returns true if there is a character immediately available from input-stream; otherwise, returns false.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .optionalParameter(INPUT_STREAM_ARGUMENT).withInitialValue(StreamVariables.STANDARD_INPUT.getVariableValue())
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final InputStreamStruct inputStreamStruct = arguments.getOptionalArgument(INPUT_STREAM_ARGUMENT, InputStreamStruct.class);
		final boolean listen = inputStreamStruct.listen();
		return LispStructFactory.toBoolean(listen);
	}
}
