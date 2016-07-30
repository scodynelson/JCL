/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.character;

import java.util.List;
import java.util.function.Predicate;

import jcl.lang.LispStruct;
import jcl.lang.TStruct;
import jcl.lang.character.CharacterStructImpl;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.FunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;
import jcl.lang.list.NILStruct;

/**
 * Abstract {@link FunctionStruct} implementation for character functions that operates one to many {@link
 * CharacterStructImpl}s to verify their equality properties.
 */
abstract class AbstractCharacterEqualityFunction extends CommonLispBuiltInFunctionStruct {

	protected AbstractCharacterEqualityFunction(final String documentation, final String functionName) {
		super(documentation, functionName,
		      Parameters.forFunction(functionName)
		                .requiredParameter("CHARACTER")
		                .restParameter()
		);
	}

	/**
	 * {@inheritDoc}
	 * Application method for the character function that gets the {@link CharacterStructImpl} parameter object and applies
	 * the result of the abstract {@link #characterEqualityPredicate()} method with the parameter as the {@link
	 * Predicate} parameter.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return the result of the {@link #characterEqualityPredicate()} applied to the {@link CharacterStructImpl} parameter
	 * value
	 */
	@Override
	public LispStruct apply(final Arguments arguments) {
		final CharacterStructImpl character = arguments.getRequiredArgument("CHARACTER", CharacterStructImpl.class);
		final List<CharacterStructImpl> characters = arguments.getRestArgument(CharacterStructImpl.class);

		// TODO: Fix this nonsense
		final CharacterStructImpl[] characterArray = new CharacterStructImpl[characters.size() + 1];
		characterArray[0] = character;
		for (int i = 1; i <= characters.size(); i++) {
			characterArray[i] = characters.get(i - 1);
		}

		return characterEqualityPredicate().test(characterArray) ? TStruct.INSTANCE : NILStruct.INSTANCE;
	}

	/**
	 * Abstract method to return a {@link Predicate} that consumes a {@link CharacterStructImpl[]}.
	 *
	 * @return returns a {@link Predicate} that consumes a {@link CharacterStructImpl[]}
	 */
	protected abstract Predicate<CharacterStructImpl[]> characterEqualityPredicate();
}
