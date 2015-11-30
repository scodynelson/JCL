/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Function;

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
	 * Private constructor passing the documentation string.
	 */
	private CharLesspFunction() {
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
	 * Returns {@link CharacterStruct#isLessThanIgnoreCase(CharacterStruct...)} as a method reference function.
	 *
	 * @return returns {@link CharacterStruct#isLessThanIgnoreCase(CharacterStruct...)} as a method reference function
	 */
	@Override
	protected Function<CharacterStruct[], Boolean> characterEqualityFunction() {
		return CharacterStruct::isLessThanIgnoreCase;
	}
}
