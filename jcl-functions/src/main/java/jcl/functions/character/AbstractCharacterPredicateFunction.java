/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.character;

import java.util.function.Predicate;

import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.character.CharacterStructImpl;
import jcl.lang.function.CommonLispBuiltInFunctionStruct;
import jcl.lang.function.FunctionStruct;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

/**
 * Abstract {@link FunctionStruct} implementation for character functions that operates on a {@link CharacterStructImpl} to
 * check against some {@link Predicate} operation.
 */
abstract class AbstractCharacterPredicateFunction extends CommonLispBuiltInFunctionStruct {

	protected AbstractCharacterPredicateFunction(final String documentation, final String functionName) {
		super(documentation, functionName,
		      Parameters.forFunction(functionName)
		                .requiredParameter("CHARACTER")
		);
	}

	/**
	 * {@inheritDoc}
	 * Application method for the character function that gets the {@link CharacterStructImpl} parameter object and applies
	 * the result of the abstract {@link #predicate()} method with the parameter as the {@link Predicate} parameter.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return the result of the {@link #predicate()} applied to the {@link CharacterStructImpl} parameter value
	 */
	@Override
	public LispStruct apply(final Arguments arguments) {
		final CharacterStructImpl character = arguments.getRequiredArgument("CHARACTER", CharacterStructImpl.class);
		return BooleanStruct.toLispBoolean(predicate().test(character));
	}

	/**
	 * Abstract method to return a {@link Predicate} that consumes a {@link CharacterStructImpl}.
	 *
	 * @return returns a {@link Predicate} that consumes a {@link CharacterStructImpl}
	 */
	protected abstract Predicate<CharacterStructImpl> predicate();
}
