/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Predicate;

import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code char-greaterp}.
 */
@Component
public final class CharGreaterpFunction extends AbstractCharacterEqualityFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public CharGreaterpFunction() {
		super("Returns true if the characters are monotonically decreasing, ignoring differences in case; otherwise, it returns false.");
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code char-greaterp} as a string.
	 *
	 * @return the function name {@code char-greaterp} as a string
	 */
	@Override
	protected String functionName() {
		return "CHAR-GREATERP";
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStruct#isGreaterThanIgnoreCase(CharacterStruct...)} as a method reference predicate.
	 *
	 * @return returns {@link CharacterStruct#isGreaterThanIgnoreCase(CharacterStruct...)} as a method reference
	 * predicate
	 */
	@Override
	protected Predicate<CharacterStruct[]> characterEqualityPredicate() {
		return CharacterStruct::isGreaterThanIgnoreCase;
	}
}
