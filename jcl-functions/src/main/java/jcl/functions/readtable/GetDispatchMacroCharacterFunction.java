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
public final class GetDispatchMacroCharacterFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "GET-DISPATCH-MACRO-CHARACTER";
	private static final String DISPATCH_CHAR_ARGUMENT = "DISPATCH-CHAR";
	private static final String SUB_CHAR_ARGUMENT = "SUB-CHAR";
	private static final String READTABLE_ARGUMENT = "READTABLE";

	public GetDispatchMacroCharacterFunction() {
		super("Retrieves the dispatch function associated with disp-char and sub-char in readtable.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(DISPATCH_CHAR_ARGUMENT)
		                .requiredParameter(SUB_CHAR_ARGUMENT)
		                .optionalParameter(READTABLE_ARGUMENT).withInitialValue(ReaderVariables.READTABLE.getVariableValue())
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final CharacterStruct dispatchChar = arguments.getRequiredArgument(DISPATCH_CHAR_ARGUMENT, CharacterStruct.class);
		final CharacterStruct subChar = arguments.getRequiredArgument(SUB_CHAR_ARGUMENT, CharacterStruct.class);
		final ReadtableStruct readtable = arguments.getOptionalArgument(READTABLE_ARGUMENT, ReadtableStruct.class);

		return readtable.getDispatchMacroCharacter(dispatchChar.toUnicodeCodePoint(), subChar.toUnicodeCodePoint());
	}
}
