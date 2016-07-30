/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.character;

import java.util.function.Predicate;

import jcl.lang.character.CharacterStructImpl;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code char-not-lessp}.
 */
@Component
public final class CharNotLesspFunction extends AbstractCharacterEqualityFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public CharNotLesspFunction() {
		super("Returns true if the characters are monotonically nonincreasing, ignoring differences in case; otherwise, it returns false.",
		      "CHAR-NOT-LESSP"
		);
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStructImpl#isGreaterThanOrEqualToIgnoreCase(CharacterStructImpl...)} as a method reference
	 * function.
	 *
	 * @return returns {@link CharacterStructImpl#isGreaterThanOrEqualToIgnoreCase(CharacterStructImpl...)} as a method
	 * reference function
	 */
	@Override
	protected Predicate<CharacterStructImpl[]> characterEqualityPredicate() {
		return CharacterStructImpl::isGreaterThanOrEqualToIgnoreCase;
	}
}
