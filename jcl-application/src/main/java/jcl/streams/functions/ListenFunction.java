/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.streams.InputStream;
import jcl.streams.StreamVariables;
import jcl.symbols.BooleanStructs;
import org.springframework.stereotype.Component;

@Component
public final class ListenFunction extends CommonLispBuiltInFunctionStruct {

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
		final InputStream inputStream = arguments.getOptionalArgument(INPUT_STREAM_ARGUMENT, InputStream.class);
		final boolean listen = inputStream.listen();
		return BooleanStructs.toLispBoolean(listen);
	}
}
