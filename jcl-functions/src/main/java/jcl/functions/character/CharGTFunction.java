/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.character;

import java.util.function.Predicate;

import jcl.lang.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code char>}.
 */
@Component
public final class CharGTFunction extends AbstractCharacterEqualityFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public CharGTFunction() {
		super("Returns true if the characters are monotonically decreasing; otherwise, it returns false.",
		      "CHAR>"
		);
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStruct#isGreaterThan(CharacterStruct...)} as a method reference predicate.
	 *
	 * @return returns {@link CharacterStruct#isGreaterThan(CharacterStruct...)} as a method reference predicate
	 */
	@Override
	protected Predicate<CharacterStruct[]> characterEqualityPredicate() {
		return CharacterStruct::isGreaterThan;
	}
}
