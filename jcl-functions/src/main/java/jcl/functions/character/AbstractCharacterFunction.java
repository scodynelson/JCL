/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.character;

import java.util.function.Function;

import jcl.lang.LispStruct;
import jcl.lang.character.CharacterStructImpl;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.FunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

/**
 * Abstract {@link FunctionStruct} implementation for character functions that operates on a {@link CharacterStructImpl}.
 */
abstract class AbstractCharacterFunction extends CommonLispBuiltInFunctionStruct {

	protected AbstractCharacterFunction(final String documentation, final String functionName) {
		super(documentation, functionName,
		      Parameters.forFunction(functionName)
		                .requiredParameter("CHARACTER")
		);
	}

	/**
	 * {@inheritDoc}
	 * Application method for the character function that gets the {@link CharacterStructImpl} parameter object and applies
	 * the result of the abstract {@link #characterFunction()} method with the parameter as the {@link Function}
	 * parameter.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return the result of the {@link #characterFunction()} applied to the {@link CharacterStructImpl} parameter value
	 */
	@Override
	public LispStruct apply(final Arguments arguments) {
		final CharacterStructImpl character = arguments.getRequiredArgument("CHARACTER", CharacterStructImpl.class);
		return characterFunction().apply(character);
	}

	/**
	 * Abstract method to return a {@link Function} that consumes a {@link CharacterStructImpl} and returns a {@link
	 * LispStruct} as a result.
	 *
	 * @return returns a {@link Function} that consumes a {@link CharacterStructImpl} and returns a {@link LispStruct} as a
	 * result
	 */
	protected abstract Function<CharacterStructImpl, LispStruct> characterFunction();
}
