/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Predicate;

import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code lower-case-p}.
 */
@Component
public final class LowerCasePFunction extends AbstractCharacterPredicateFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public LowerCasePFunction() {
		super("Returns true if character is a lowercase character; otherwise, returns false.",
		      "LOWER-CASE-P"
		);
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStruct#isLowerCase()} as a method reference predicate.
	 *
	 * @return returns {@link CharacterStruct#isLowerCase()} as a method reference predicate
	 */
	@Override
	protected Predicate<CharacterStruct> predicate() {
		return CharacterStruct::isLowerCase;
	}
}
