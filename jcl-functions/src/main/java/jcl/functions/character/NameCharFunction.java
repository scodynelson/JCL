/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.character;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.CharacterStruct;
import jcl.lang.LispStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code name-char}.
 */
@Component
public final class NameCharFunction extends CommonLispBuiltInFunctionStructBase {

	private static final String FUNCTION_NAME = "NAME-CHAR";

	/**
	 * Public constructor passing the documentation string.
	 */
	public NameCharFunction() {
		super("Returns the character object whose name is name. If such a character does not exist, nil is returned.",
		      FUNCTION_NAME,
		      Parameters.forFunction(FUNCTION_NAME)
		                .requiredParameter("CHARACTER-DESIGNATOR")
		);
	}

	@Override
	public LispStruct apply(final Arguments arguments) {
		final LispStruct characterDesignator = arguments.getRequiredArgument("CHARACTER-DESIGNATOR");
		return CharacterStruct.nameChar(characterDesignator);
	}
}
