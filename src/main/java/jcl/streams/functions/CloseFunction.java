/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import jcl.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.streams.StreamStruct;
import jcl.symbols.BooleanStruct;
import jcl.symbols.BooleanStructs;
import jcl.symbols.NILStruct;
import jcl.system.CommonLispSymbols;
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
		return BooleanStructs.toLispBoolean(wasClosed);
	}
}
