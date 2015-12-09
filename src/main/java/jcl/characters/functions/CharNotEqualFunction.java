/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Predicate;

import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code char-not-equal}.
 */
@Component
public final class CharNotEqualFunction extends AbstractCharacterEqualityFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -88330282287626428L;

	/**
	 * Public constructor passing the documentation string.
	 */
	public CharNotEqualFunction() {
		super("Returns true if all characters are different, ignoring differences in case; otherwise, it returns false.");
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code char-not-equal} as a string.
	 *
	 * @return the function name {@code char-not-equal} as a string
	 */
	@Override
	protected String functionName() {
		return "CHAR-NOT-EQUAL";
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
