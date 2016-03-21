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
import jcl.symbols.NILStruct;
import org.springframework.stereotype.Component;

@Component
public final class TerpriFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "TERPRI";
	private static final String OUTPUT_STREAM_ARGUMENT = "OUTPUT-STREAM";

	public TerpriFunction() {
		super("Outputs a newline to output-stream.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .optionalParameter(OUTPUT_STREAM_ARGUMENT).withInitialValue(StreamVariables.STANDARD_OUTPUT.getVariableValue())
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final OutputStream outputStream = arguments.getOptionalArgument(OUTPUT_STREAM_ARGUMENT, OutputStream.class);

		outputStream.writeChar('\n');
		return NILStruct.INSTANCE;
	}
}
