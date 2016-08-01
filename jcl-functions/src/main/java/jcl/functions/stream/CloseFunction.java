/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import jcl.lang.BooleanStruct;
import jcl.lang.CommonLispSymbols;
import jcl.lang.LispStruct;
import jcl.lang.StreamStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.list.NILStruct;
import org.springframework.stereotype.Component;

@Component
public final class CloseFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "CLOSE";
	private static final String STREAM_ARGUMENT = "STREAM";

	public CloseFunction() {
		super("Closes Stream.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(STREAM_ARGUMENT)
		                .keyParameter(CommonLispSymbols.ABORT_KEYWORD).withInitialValue(NILStruct.INSTANCE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final StreamStruct stream = arguments.getRequiredArgument(STREAM_ARGUMENT, StreamStruct.class);
		final BooleanStruct abortValue = arguments.getKeyArgument(CommonLispSymbols.ELEMENT_TYPE_KEYWORD, BooleanStruct.class);

		final boolean wasClosed = stream.close(abortValue.booleanValue());
		return BooleanStruct.toLispBoolean(wasClosed);
	}
}
