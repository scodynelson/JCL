/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.stream;

import jcl.lang.CharacterStruct;
import jcl.lang.LispStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.OutputStreamStruct;
import jcl.lang.statics.StreamVariables;
import org.springframework.stereotype.Component;

@Component
public final class WriteCharFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "WRITE-CHAR";
	private static final String CHARACTER_ARGUMENT = "CHARACTER";
	private static final String OUTPUT_STREAM_ARGUMENT = "OUTPUT-STREAM";

	public WriteCharFunction() {
		super("Outputs character to output-stream.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(CHARACTER_ARGUMENT)
		                .optionalParameter(OUTPUT_STREAM_ARGUMENT).withInitialValue(StreamVariables.STANDARD_OUTPUT.getVariableValue())
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final CharacterStruct character = arguments.getRequiredArgument(CHARACTER_ARGUMENT, CharacterStruct.class);
		final OutputStreamStruct outputStreamStruct = arguments.getOptionalArgument(OUTPUT_STREAM_ARGUMENT, OutputStreamStruct.class);

		outputStreamStruct.writeChar(character.getCodePoint());
		return character;
	}
}
