/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.readtable;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.CharacterStruct;
import jcl.lang.LispStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.ReaderVariables;
import org.springframework.stereotype.Component;

@Component
public final class GetMacroCharacterFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "GET-MACRO-CHARACTER";
	private static final String CHAR_ARGUMENT = "CHAR";
	private static final String READTABLE_ARGUMENT = "READTABLE";

	public GetMacroCharacterFunction() {
		super("Returns the reader macro function associated with char in readtable.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(CHAR_ARGUMENT)
		                .optionalParameter(READTABLE_ARGUMENT).withInitialValue(ReaderVariables.READTABLE.getVariableValue())
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final CharacterStruct character = arguments.getRequiredArgument(CHAR_ARGUMENT, CharacterStruct.class);
		final ReadtableStruct readtable = arguments.getOptionalArgument(READTABLE_ARGUMENT, ReadtableStruct.class);

		return readtable.getMacroCharacter(character.getCodePoint());
	}
}
