/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import jcl.lang.LispStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.InputStreamStruct;
import jcl.lang.OutputStreamStruct;
import org.springframework.stereotype.Component;

@Component
public final class MakeTwoWayStreamFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "MAKE-TWO-WAY-STREAM";
	private static final String INPUT_STREAM_ARGUMENT = "INPUT-STREAM";
	private static final String OUTPUT_STREAM_ARGUMENT = "OUTPUT-STREAM";

	public MakeTwoWayStreamFunction() {
		super("Returns a two-way stream that gets its input from input-stream and sends its output to output-stream.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(INPUT_STREAM_ARGUMENT)
		                .requiredParameter(OUTPUT_STREAM_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final InputStreamStruct inputStreamStruct = arguments.getRequiredArgument(INPUT_STREAM_ARGUMENT, InputStreamStruct.class);
		final OutputStreamStruct outputStreamStruct = arguments.getRequiredArgument(OUTPUT_STREAM_ARGUMENT, OutputStreamStruct.class);
		return LispStructFactory.toTwoWayStream(inputStreamStruct, outputStreamStruct);
	}
}
