/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.character;

import java.util.function.Predicate;

import jcl.lang.CharacterStruct;
import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.functions.CommonLispBuiltInFunctionStructBase;
import jcl.lang.function.parameterdsl.Arguments;
import jcl.lang.function.parameterdsl.Parameters;

/**
 * Abstract {@link FunctionStruct} implementation for character functions that operates on a {@link CharacterStruct} to
 * check against some {@link Predicate} operation.
 */
abstract class AbstractCharacterPredicateFunction extends CommonLispBuiltInFunctionStructBase {

	protected AbstractCharacterPredicateFunction(final String documentation, final String functionName) {
		super(documentation, functionName,
		      Parameters.forFunction(functionName)
		                .requiredParameter("CHARACTER")
		);
	}

	/**
	 * {@inheritDoc}
	 * Application method for the character function that gets the {@link CharacterStruct} parameter object and applies
	 * the result of the abstract {@link #predicate()} method with the parameter as the {@link Predicate} parameter.
	 *
	 * @param lispStructs
	 * 		the function parameters
	 *
	 * @return the result of the {@link #predicate()} applied to the {@link CharacterStruct} parameter value
	 */
	@Override
	public LispStruct apply(final Arguments arguments) {
		final CharacterStruct character = arguments.getRequiredArgument("CHARACTER", CharacterStruct.class);
		return LispStructFactory.toBoolean(predicate().test(character));
	}

	/**
	 * Abstract method to return a {@link Predicate} that consumes a {@link CharacterStruct}.
	 *
	 * @return returns a {@link Predicate} that consumes a {@link CharacterStruct}
	 */
	protected abstract Predicate<CharacterStruct> predicate();
}
