/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams.functions;

import jcl.lang.CommonLispSymbols;
import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.stream.StringOutputStreamStruct;
import jcl.type.CharacterType;
import jcl.type.LispType;
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
