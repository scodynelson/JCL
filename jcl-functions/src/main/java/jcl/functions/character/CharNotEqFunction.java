/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.character;

import java.util.function.Predicate;

import jcl.lang.character.CharacterStructImpl;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code char/=}.
 */
@Component
public final class CharNotEqFunction extends AbstractCharacterEqualityFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public CharNotEqFunction() {
		super("Returns true if all characters are different; otherwise, it returns false.",
		      "CHAR/="
		);
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStructImpl#isNotEqualTo(CharacterStructImpl...)} as a method reference predicate.
	 *
	 * @return returns {@link CharacterStructImpl#isNotEqualTo(CharacterStructImpl...)} as a method reference predicate
	 */
	@Override
	protected Predicate<CharacterStructImpl[]> characterEqualityPredicate() {
		return CharacterStructImpl::isNotEqualTo;
	}
}
