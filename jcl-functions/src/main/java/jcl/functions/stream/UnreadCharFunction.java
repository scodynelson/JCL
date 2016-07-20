/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import jcl.lang.LispStruct;
import jcl.lang.character.CharacterStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.stream.InputStream;
import jcl.lang.stream.StreamVariables;
import org.springframework.stereotype.Component;

@Component
public final class UnreadCharFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "UNREAD-CHAR";
	private static final String CHARACTER_ARGUMENT = "CHARACTER";
	private static final String INPUT_STREAM_ARGUMENT = "INPUT-STREAM";

	public UnreadCharFunction() {
		super("Places character back onto the front of input-stream so that it will again be the next character in input-stream.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(CHARACTER_ARGUMENT)
		                .optionalParameter(INPUT_STREAM_ARGUMENT).withInitialValue(StreamVariables.STANDARD_INPUT.getVariableValue())
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final CharacterStruct character = arguments.getRequiredArgument(CHARACTER_ARGUMENT, CharacterStruct.class);
		final InputStream inputStream = arguments.getOptionalArgument(INPUT_STREAM_ARGUMENT, InputStream.class);

		inputStream.unreadChar(character.getCodePoint());
		return character;
	}
}
