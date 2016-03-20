/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.symbols.BooleanStructs;
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
		return BooleanStructs.toLispBoolean(object instanceof CharacterStruct);
	}
}
