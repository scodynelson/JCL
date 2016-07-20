/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.character;

import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.character.CharacterStruct;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code characterp}.
 */
@Component
public final class CharacterPFunction extends CommonLispBuiltInFunctionStruct {

	private static final String FUNCTION_NAME = "CHARACTERP";
	private static final String OBJECT_ARGUMENT = "OBJECT";

	public CharacterPFunction() {
		super("Returns true if object is of type character; otherwise, returns false.",
		      "CHARACTERP",
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter(OBJECT_ARGUMENT)
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct object = arguments.getRequiredArgument(OBJECT_ARGUMENT);
		return BooleanStruct.toLispBoolean(object instanceof CharacterStruct);
	}
}
