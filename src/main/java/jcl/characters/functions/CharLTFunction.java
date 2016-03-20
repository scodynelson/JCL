/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Predicate;

import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code char<}.
 */
@Component
public final class CharLTFunction extends AbstractCharacterEqualityFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public CharLTFunction() {
		super("Returns true if the characters are monotonically increasing; otherwise, it returns false.",
		      "CHAR<"
		);
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStruct#isLessThan(CharacterStruct...)} as a method reference predicate.
	 *
	 * @return returns {@link CharacterStruct#isLessThan(CharacterStruct...)} as a method reference predicate
	 */
	@Override
	protected Predicate<CharacterStruct[]> characterEqualityPredicate() {
		return CharacterStruct::isLessThan;
	}
}
