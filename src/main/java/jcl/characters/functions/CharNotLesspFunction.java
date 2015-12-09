/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Predicate;

import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code char-not-lessp}.
 */
@Component
public final class CharNotLesspFunction extends AbstractCharacterEqualityFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -302980532887992947L;

	/**
	 * Public constructor passing the documentation string.
	 */
	public CharNotLesspFunction() {
		super("Returns true if the characters are monotonically nonincreasing, ignoring differences in case; otherwise, it returns false.");
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code char-not-lessp} as a string.
	 *
	 * @return the function name {@code char-not-lessp} as a string
	 */
	@Override
	protected String functionName() {
		return "CHAR-NOT-LESSP";
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStruct#isGreaterThanOrEqualToIgnoreCase(CharacterStruct...)} as a method reference
	 * function.
	 *
	 * @return returns {@link CharacterStruct#isGreaterThanOrEqualToIgnoreCase(CharacterStruct...)} as a method
	 * reference function
	 */
	@Override
	protected Predicate<CharacterStruct[]> characterEqualityPredicate() {
		return CharacterStruct::isGreaterThanOrEqualToIgnoreCase;
	}
}
