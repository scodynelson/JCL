/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Predicate;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.FunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.symbols.BooleanStructs;

/**
 * Abstract {@link FunctionStruct} implementation for character functions that operates on a {@link CharacterStruct} to
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
		return BooleanStructs.toLispBoolean(predicate().test(character));
	}

	/**
	 * Abstract method to return a {@link Predicate} that consumes a {@link CharacterStruct}.
	 *
	 * @return returns a {@link Predicate} that consumes a {@link CharacterStruct}
	 */
	protected abstract Predicate<CharacterStruct> predicate();
}
