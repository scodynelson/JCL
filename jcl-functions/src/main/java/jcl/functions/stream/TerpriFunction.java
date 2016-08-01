/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.list.NILStruct;
import jcl.lang.OutputStreamStruct;
import jcl.lang.stream.StreamVariables;
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
		final OutputStreamStruct outputStreamStruct = arguments.getOptionalArgument(OUTPUT_STREAM_ARGUMENT, OutputStreamStruct.class);

		outputStreamStruct.writeChar('\n');
		return NILStruct.INSTANCE;
	}
}
