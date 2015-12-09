/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Predicate;

import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code char-lessp}.
 */
@Component
public final class CharLesspFunction extends AbstractCharacterEqualityFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -8873603818879068098L;

	/**
	 * Public constructor passing the documentation string.
	 */
	public CharLesspFunction() {
		super("Returns true if the characters are monotonically increasing, ignoring differences in case; otherwise, it returns false.");
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code char-lessp} as a string.
	 *
	 * @return the function name {@code char-lessp} as a string
	 */
	@Override
	protected String functionName() {
		return "CHAR-LESSP";
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStruct#isLessThanIgnoreCase(CharacterStruct...)} as a method reference predicate.
	 *
	 * @return returns {@link CharacterStruct#isLessThanIgnoreCase(CharacterStruct...)} as a method reference predicate
	 */
	@Override
	protected Predicate<CharacterStruct[]> characterEqualityPredicate() {
		return CharacterStruct::isLessThanIgnoreCase;
	}
}
