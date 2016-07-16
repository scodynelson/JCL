/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.character;

import java.util.function.Predicate;

import jcl.lang.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code char<=}.
 */
@Component
public final class CharLTEqFunction extends AbstractCharacterEqualityFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public CharLTEqFunction() {
		super("Returns true if the characters are monotonically nondecreasing; otherwise, it returns false.",
		      "CHAR<="
		);
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStruct#isLessThanOrEqualTo(CharacterStruct...)} as a method reference predicate.
	 *
	 * @return returns {@link CharacterStruct#isLessThanOrEqualTo(CharacterStruct...)} as a method reference predicate
	 */
	@Override
	protected Predicate<CharacterStruct[]> characterEqualityPredicate() {
		return CharacterStruct::isLessThanOrEqualTo;
	}
}
