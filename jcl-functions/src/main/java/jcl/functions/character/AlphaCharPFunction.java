/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.character;

import java.util.function.Predicate;

import jcl.lang.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code alpha-char-p}.
 */
@Component
public final class AlphaCharPFunction extends AbstractCharacterPredicateFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public AlphaCharPFunction() {
		super("Returns true if character is an alphabetic character; otherwise, returns false.",
		      "ALPHA-CHAR-P"
		);
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStruct#isAlphaChar()} as a method reference predicate.
	 *
	 * @return returns {@link CharacterStruct#isAlphaChar()} as a method reference predicate
	 */
	@Override
	protected Predicate<CharacterStruct> predicate() {
		return CharacterStruct::isAlphaChar;
	}
}