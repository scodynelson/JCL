/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import jcl.LispStruct;
import jcl.LispType;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.streams.StringOutputStreamStruct;
import jcl.system.CommonLispSymbols;
import jcl.types.CharacterType;
import org.springframework.stereotype.Component;

@Component
public final class MakeStringOutputStreamFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "MAKE-STRING-OUTPUT-STREAM";

	public MakeStringOutputStreamFunction() {
		super("Returns an output string stream that accepts characters and makes available (via get-output-stream-string) a string that contains the characters that were actually output.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .keyParameter(CommonLispSymbols.ELEMENT_TYPE_KEYWORD).withInitialValue(CharacterType.INSTANCE)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispType elementType = arguments.getKeyArgument(CommonLispSymbols.ELEMENT_TYPE_KEYWORD, LispType.class);
		return new StringOutputStreamStruct(elementType);
	}
}
