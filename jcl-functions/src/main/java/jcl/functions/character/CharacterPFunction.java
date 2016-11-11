/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.character;

import jcl.lang.CharacterStruct;
import jcl.lang.LispStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code characterp}.
 */
@Component
public final class CharacterPFunction extends CommonLispBuiltInFunctionStructBase {

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
		return LispStructFactory.toBoolean(object instanceof CharacterStruct);
	}
}
