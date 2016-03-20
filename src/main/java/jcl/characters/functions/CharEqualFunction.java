/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Predicate;

import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code char-equal}.
 */
@Component
public final class CharEqualFunction extends AbstractCharacterEqualityFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public CharEqualFunction() {
		super("Returns true if all characters are the same, ignoring differences in case; otherwise, it returns false.",
		      "CHAR-EQUAL"
		);
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStruct#isEqualToIgnoreCase(CharacterStruct...)} as a method reference predicate.
	 *
	 * @return returns {@link CharacterStruct#isEqualToIgnoreCase(CharacterStruct...)} as a method reference predicate
	 */
	@Override
	protected Predicate<CharacterStruct[]> characterEqualityPredicate() {
		return CharacterStruct::isEqualToIgnoreCase;
	}
}
