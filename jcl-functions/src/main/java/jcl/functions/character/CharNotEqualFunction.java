/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.character;

import java.util.function.Predicate;

import jcl.lang.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code char-not-equal}.
 */
@Component
public final class CharNotEqualFunction extends AbstractCharacterEqualityFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public CharNotEqualFunction() {
		super("Returns true if all characters are different, ignoring differences in case; otherwise, it returns false.",
		      "CHAR-NOT-EQUAL"
		);
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStruct#isNotEqualToIgnoreCase(CharacterStruct...)} as a method reference predicate.
	 *
	 * @return returns {@link CharacterStruct#isNotEqualToIgnoreCase(CharacterStruct...)} as a method reference
	 * predicate
	 */
	@Override
	protected Predicate<CharacterStruct[]> characterEqualityPredicate() {
		return CharacterStruct::isNotEqualToIgnoreCase;
	}
}
