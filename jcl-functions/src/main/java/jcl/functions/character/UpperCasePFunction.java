/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.character;

import java.util.function.Predicate;

import jcl.lang.character.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code upper-case-p}.
 */
@Component
public final class UpperCasePFunction extends AbstractCharacterPredicateFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public UpperCasePFunction() {
		super("Returns true if character is an uppercase character; otherwise, returns false.",
		      "UPPER-CASE-P"
		);
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStruct#isUpperCase()} as a method reference predicate.
	 *
	 * @return returns {@link CharacterStruct#isUpperCase()} as a method reference predicate
	 */
	@Override
	protected Predicate<CharacterStruct> predicate() {
		return CharacterStruct::isUpperCase;
	}
}
