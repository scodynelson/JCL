/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.character;

import java.util.function.Predicate;

import jcl.lang.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code char-not-greaterp}.
 */
@Component
public final class CharNotGreaterpFunction extends AbstractCharacterEqualityFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public CharNotGreaterpFunction() {
		super("Returns true if the characters are monotonically nondecreasing, ignoring differences in case; otherwise, it returns false.",
		      "CHAR-NOT-GREATERP"
		);
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStruct#isLessThanOrEqualToIgnoreCase(CharacterStruct...)} as a method reference
	 * function.
	 *
	 * @return returns {@link CharacterStruct#isLessThanOrEqualToIgnoreCase(CharacterStruct...)} as a method reference
	 * function
	 */
	@Override
	protected Predicate<CharacterStruct[]> characterEqualityPredicate() {
		return CharacterStruct::isLessThanOrEqualToIgnoreCase;
	}
}
