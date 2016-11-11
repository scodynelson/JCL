/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.LispStruct;
import jcl.lang.OutputStreamStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.StreamVariables;
import org.springframework.stereotype.Component;

@Component
public final class FreshLineFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "FRESHLINE";
	private static final String OUTPUT_STREAM_ARGUMENT = "OUTPUT-STREAM";

	public FreshLineFunction() {
		super("Outputs a newline only if the output-stream is not already at the start of a line.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .optionalParameter(OUTPUT_STREAM_ARGUMENT).withInitialValue(StreamVariables.STANDARD_OUTPUT.getVariableValue())
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final OutputStreamStruct outputStreamStruct = arguments.getOptionalArgument(OUTPUT_STREAM_ARGUMENT, OutputStreamStruct.class);

		final boolean shouldWriteNewline = !outputStreamStruct.isStartOfLine();
		if (shouldWriteNewline) {
			outputStreamStruct.writeChar('\n');
		}

		return LispStructFactory.toBoolean(shouldWriteNewline);
	}
}
