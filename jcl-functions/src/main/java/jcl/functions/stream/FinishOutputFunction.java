/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.stream.OutputStream;
import jcl.lang.stream.StreamVariables;
import org.springframework.stereotype.Component;

@Component
public final class FinishOutputFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "FINISH-OUTPUT";
	private static final String OUTPUT_STREAM_ARGUMENT = "OUTPUT-STREAM";

	public FinishOutputFunction() {
		super("Attempts to ensure that any buffered output sent to output-stream has reached its destination, and then returns.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .optionalParameter(OUTPUT_STREAM_ARGUMENT).withInitialValue(StreamVariables.STANDARD_OUTPUT.getVariableValue())
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final OutputStream outputStream = arguments.getOptionalArgument(OUTPUT_STREAM_ARGUMENT, OutputStream.class);
		outputStream.finishOutput();
		return NILStruct.INSTANCE;
	}
}
