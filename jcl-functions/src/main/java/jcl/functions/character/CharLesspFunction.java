/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.character;

import java.util.function.Predicate;

import jcl.lang.character.CharacterStructImpl;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code char-lessp}.
 */
@Component
public final class CharLesspFunction extends AbstractCharacterEqualityFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public CharLesspFunction() {
		super("Returns true if the characters are monotonically increasing, ignoring differences in case; otherwise, it returns false.",
		      "CHAR-LESSP"
		);
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStructImpl#isLessThanIgnoreCase(CharacterStructImpl...)} as a method reference predicate.
	 *
	 * @return returns {@link CharacterStructImpl#isLessThanIgnoreCase(CharacterStructImpl...)} as a method reference predicate
	 */
	@Override
	protected Predicate<CharacterStructImpl[]> characterEqualityPredicate() {
		return CharacterStructImpl::isLessThanIgnoreCase;
	}
}
