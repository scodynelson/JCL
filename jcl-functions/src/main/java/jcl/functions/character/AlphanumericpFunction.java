/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.character;

import java.util.function.Predicate;

import jcl.lang.character.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code alphanumericp}.
 */
@Component
public final class AlphanumericpFunction extends AbstractCharacterPredicateFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public AlphanumericpFunction() {
		super("Returns true if character is an alphabetic character or a numeric character; otherwise, returns false.",
		      "ALPHANUMERICP"
		);
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStruct#isAlphanumeric()} as a method reference predicate.
	 *
	 * @return returns {@link CharacterStruct#isAlphanumeric()} as a method reference predicate
	 */
	@Override
	protected Predicate<CharacterStruct> predicate() {
		return CharacterStruct::isAlphanumeric;
	}
}
