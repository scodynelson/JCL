/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.character;

import java.util.function.Predicate;

import jcl.lang.character.CharacterStructImpl;
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
	 * Returns {@link CharacterStructImpl#isLessThanOrEqualToIgnoreCase(CharacterStructImpl...)} as a method reference
	 * function.
	 *
	 * @return returns {@link CharacterStructImpl#isLessThanOrEqualToIgnoreCase(CharacterStructImpl...)} as a method reference
	 * function
	 */
	@Override
	protected Predicate<CharacterStructImpl[]> characterEqualityPredicate() {
		return CharacterStructImpl::isLessThanOrEqualToIgnoreCase;
	}
}
