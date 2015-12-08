/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Predicate;

import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code char/=}.
 */
@Component
public final class CharNotEqFunction extends AbstractCharacterEqualityFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -4521919308468314716L;

	/**
	 * Private constructor passing the documentation string.
	 */
	private CharNotEqFunction() {
		super("Returns true if all characters are different; otherwise, it returns false.");
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code char/=} as a string.
	 *
	 * @return the function name {@code char/=} as a string
	 */
	@Override
	protected String functionName() {
		return "CHAR/=";
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStruct#isNotEqualTo(CharacterStruct...)} as a method reference predicate.
	 *
	 * @return returns {@link CharacterStruct#isNotEqualTo(CharacterStruct...)} as a method reference predicate
	 */
	@Override
	protected Predicate<CharacterStruct[]> characterEqualityPredicate() {
		return CharacterStruct::isNotEqualTo;
	}
}
