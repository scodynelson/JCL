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
import jcl.functions.AbstractCommonLispFunctionStruct;
import jcl.functions.FunctionStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.NILStruct;
import jcl.symbols.TStruct;
import jcl.types.CharacterType;
import jcl.types.TypeValidator;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * Abstract {@link FunctionStruct} implementation for character functions that operates one to many {@link
 * CharacterStruct}s to verify their equality properties.
 */
abstract class AbstractCharacterEqualityFunction extends AbstractCommonLispFunctionStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 3117929060088318079L;

	/**
	 * The {@link TypeValidator} for validating the function parameter value types.
	 */
	@Autowired
	private TypeValidator validator;

	/**
	 * Protected constructor passing the provided {@code documentation} string to the super constructor.
	 *
	 * @param documentation
	 * 		the documentation string
	 */
	protected AbstractCharacterEqualityFunction(final String documentation) {
		super(documentation);
	}

	/**
	 * {@inheritDoc}
	 * Creates the single {@link RequiredParameter} character object for this function.
	 *
	 * @return a list of a single {@link RequiredParameter} character object
	 */
	@Override
	protected List<RequiredParameter> getRequiredBindings() {
		return new RequiredParameter.Builder(GlobalPackageStruct.COMMON_LISP, "CHARACTER").buildList();
	}

	/**
	 * {@inheritDoc}
	 * Creates the {@link RestParameter} to contain the remaining character objects for this function.
	 *
	 * @return the {@link RestParameter} to contain the remaining character objects
	 */
	@Override
	protected RestParameter getRestBinding() {
		return new RestParameter.Builder(GlobalPackageStruct.COMMON_LISP, "CHARACTERS").build();
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
	public LispStruct apply(final LispStruct... lispStructs) {
		super.apply(lispStructs);

		final CharacterStruct[] characters = getCharacters(lispStructs);
		return characterEqualityPredicate().test(characters) ? TStruct.INSTANCE : NILStruct.INSTANCE;
	}

	/**
	 * Gets the {@link CharacterStruct[]} from the provided {@link LispStruct[]} parameters, verifying that each object
	 * is a {@link CharacterStruct}.
	 *
	 * @param lispStructs
	 * 		the parameters to validate are {@link CharacterStruct}s
	 *
	 * @return the {@link CharacterStruct[]} from the provided {@link LispStruct[]} parameters
	 */
	private CharacterStruct[] getCharacters(final LispStruct... lispStructs) {

		final CharacterStruct[] characters = new CharacterStruct[lispStructs.length];
		for (int i = 0; i < lispStructs.length; i++) {
			final LispStruct lispStruct = lispStructs[i];
			validator.validateTypes(lispStruct, functionName(), "Character", CharacterType.INSTANCE);
			characters[i] = (CharacterStruct) lispStruct;
		}
		return characters;
	}

	/**
	 * Abstract method to return a {@link Predicate} that consumes a {@link CharacterStruct[]}.
	 *
	 * @return returns a {@link Predicate} that consumes a {@link CharacterStruct[]}
	 */
	protected abstract Predicate<CharacterStruct[]> characterEqualityPredicate();
}
