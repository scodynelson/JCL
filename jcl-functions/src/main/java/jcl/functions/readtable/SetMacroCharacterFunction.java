/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.readtable;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.BooleanStruct;
import jcl.lang.CharacterStruct;
import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.TStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.statics.ReaderVariables;
import org.springframework.stereotype.Component;

@Component
public final class SetMacroCharacterFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "SET-MACRO-CHARACTER";
	private static final String CHAR_ARGUMENT = "CHAR";
	private static final String NEW_FUNCTION_ARGUMENT = "NEW_FUNCTION";
	private static final String NON_TERMINATING_P_ARGUMENT = "NON-TERMINATING-P";
	private static final String READTABLE_ARGUMENT = "READTABLE";

	public SetMacroCharacterFunction() {
		super("Causes char to be a macro character associated with the reader macro function new-function in readtable.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(CHAR_ARGUMENT)
		                .requiredParameter(NEW_FUNCTION_ARGUMENT)
		                .optionalParameter(NON_TERMINATING_P_ARGUMENT).withInitialValue(NILStruct.INSTANCE)
		                .optionalParameter(READTABLE_ARGUMENT).withInitialValue(ReaderVariables.READTABLE.getVariableValue())
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final CharacterStruct character = arguments.getRequiredArgument(CHAR_ARGUMENT, CharacterStruct.class);
		final FunctionStruct newFunction = arguments.getRequiredArgument(NEW_FUNCTION_ARGUMENT, FunctionStruct.class);
		final BooleanStruct nonTerminatingP = arguments.getOptionalArgument(NON_TERMINATING_P_ARGUMENT, BooleanStruct.class);
		final ReadtableStruct readtable = arguments.getOptionalArgument(READTABLE_ARGUMENT, ReadtableStruct.class);

		readtable.setMacroCharacter(character.toUnicodeCodePoint(), newFunction, nonTerminatingP.booleanValue());
		return TStruct.INSTANCE;
	}
}
