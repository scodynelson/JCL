/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.character;

import java.util.List;
import java.util.function.Function;
import java.util.function.Predicate;

import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.BooleanStruct;
import jcl.lang.CharacterStruct;
import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

/**
 * Abstract {@link FunctionStruct} implementation for character functions that operates one to many {@link
 * CharacterStruct}s to verify their equality properties.
 */
abstract class AbstractCharacterEqualityFunction extends CommonLispBuiltInFunctionStructBase {

	protected AbstractCharacterEqualityFunction(final String documentation, final String functionName) {
		super(documentation, functionName,
		      Parameters.forFunction(functionName)
		                .requiredParameter("CHARACTER")
		                .restParameter()
		);
	}

	/**
	 * {@inheritDoc}
	 * Application method for the character function that gets the {@link CharacterStruct} parameter object and applies
	 * the result of the abstract {@link #characterEqualityPredicate()} method with the parameter as the {@link
	 * Predicate} parameter.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return the result of the {@link #characterEqualityPredicate()} applied to the {@link CharacterStruct} parameter
	 * value
	 */
	@Override
	public LispStruct apply(final Arguments arguments) {
		final CharacterStruct character = arguments.getRequiredArgument("CHARACTER", CharacterStruct.class);
		final List<CharacterStruct> characters = arguments.getRestArgument(CharacterStruct.class);

		// TODO: Fix this nonsense
		final CharacterStruct[] characterArray = new CharacterStruct[characters.size() + 1];
		characterArray[0] = character;
		for (int i = 1; i <= characters.size(); i++) {
			characterArray[i] = characters.get(i - 1);
		}

		return characterEqualityPredicate().apply(characterArray);
	}

	/**
	 * Abstract method to return a {@link Predicate} that consumes a {@link CharacterStruct[]}.
	 *
	 * @return returns a {@link Predicate} that consumes a {@link CharacterStruct[]}
	 */
	protected abstract Function<CharacterStruct[], BooleanStruct> characterEqualityPredicate();
}
