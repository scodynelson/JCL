/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.streams.OutputStream;
import jcl.streams.StreamVariables;
import jcl.symbols.BooleanStructs;
import org.springframework.stereotype.Component;

@Component
public final class FreshLineFunction extends CommonLispBuiltInFunctionStruct {

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
		final OutputStream outputStream = arguments.getOptionalArgument(OUTPUT_STREAM_ARGUMENT, OutputStream.class);

		final boolean shouldWriteNewline = !outputStream.isStartOfLine();
		if (shouldWriteNewline) {
			outputStream.writeChar('\n');
		}

		return BooleanStructs.toLispBoolean(shouldWriteNewline);
	}
}
