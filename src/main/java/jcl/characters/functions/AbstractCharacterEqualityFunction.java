/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.List;
import java.util.function.Predicate;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import jcl.compiler.environment.binding.lambdalist.RequiredParameter;
import jcl.compiler.environment.binding.lambdalist.RestParameter;
import jcl.functions.CommonLispBuiltInFunctionStruct;
import jcl.functions.FunctionStruct;
import jcl.functions.parameterdsl.Arguments;
import jcl.functions.parameterdsl.Parameters;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.TStruct;

/**
 * Abstract {@link FunctionStruct} implementation for character functions that operates one to many {@link
 * CharacterStruct}s to verify their equality properties.
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
	 * Creates the single {@link RequiredParameter} character object for this function.
	 *
	 * @return a list of a single {@link RequiredParameter} character object
	 */
	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return RequiredParameter.builder(GlobalPackageStruct.COMMON_LISP, "CHARACTER").buildList();
	}

	/**
	 * {@inheritDoc}
	 * Creates the {@link RestParameter} to contain the remaining character objects for this function.
	 *
	 * @return the {@link RestParameter} to contain the remaining character objects
	 */
	@Override
	protected RestParameter getRestBinding() {
		return RestParameter.builder(GlobalPackageStruct.COMMON_LISP, "CHARACTERS").build();
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

		return characterEqualityPredicate().test(characterArray) ? TStruct.INSTANCE : NILStruct.INSTANCE;
	}

	/**
	 * Abstract method to return a {@link Predicate} that consumes a {@link CharacterStruct[]}.
	 *
	 * @return returns a {@link Predicate} that consumes a {@link CharacterStruct[]}
	 */
	protected abstract Predicate<CharacterStruct[]> characterEqualityPredicate();
}
