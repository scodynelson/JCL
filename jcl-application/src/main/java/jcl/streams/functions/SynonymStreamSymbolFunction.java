/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.stream.SynonymStreamStruct;
import org.springframework.stereotype.Component;

@Component
public final class SynonymStreamSymbolFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "SYNONYM-STREAM-SYMBOL";
	private static final String SYNONYM_STREAM_ARGUMENT = "SYNONYM-STREAM";

	public SynonymStreamSymbolFunction() {
		super("Returns the symbol whose symbol-value the synonym-stream is using.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(SYNONYM_STREAM_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final SynonymStreamStruct synonymStream = arguments.getRequiredArgument(SYNONYM_STREAM_ARGUMENT, SynonymStreamStruct.class);
		return synonymStream.getSymbol();
	}
}
