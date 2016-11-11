/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.character;

import java.util.function.Function;

import jcl.lang.CharacterStruct;
import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

/**
 * Abstract {@link FunctionStruct} implementation for character functions that operates on a {@link CharacterStruct}.
 */
abstract class AbstractCharacterFunction extends CommonLispBuiltInFunctionStructBase {

	protected AbstractCharacterFunction(final String documentation, final String functionName) {
		super(documentation, functionName,
		      Parameters.forFunction(functionName)
		                .requiredParameter("CHARACTER")
		);
	}

	/**
	 * {@inheritDoc}
	 * Application method for the character function that gets the {@link CharacterStruct} parameter object and applies
	 * the result of the abstract {@link #characterFunction()} method with the parameter as the {@link Function}
	 * parameter.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return the result of the {@link #characterFunction()} applied to the {@link CharacterStruct} parameter value
	 */
	@Override
	public LispStruct apply(final Arguments arguments) {
		final CharacterStruct character = arguments.getRequiredArgument("CHARACTER", CharacterStruct.class);
		return characterFunction().apply(character);
	}

	/**
	 * Abstract method to return a {@link Function} that consumes a {@link CharacterStruct} and returns a {@link
	 * LispStruct} as a result.
	 *
	 * @return returns a {@link Function} that consumes a {@link CharacterStruct} and returns a {@link LispStruct} as a
	 * result
	 */
	protected abstract Function<CharacterStruct, LispStruct> characterFunction();
}
