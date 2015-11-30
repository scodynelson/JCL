/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Function;

import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code char-equal}.
 */
@Component
public final class CharEqualFunction extends AbstractCharacterEqualityFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 1671077613128290225L;

	/**
	 * Private constructor passing the documentation string.
	 */
	private CharEqualFunction() {
		super("Returns true if all characters are the same, ignoring differences in case; otherwise, it returns false.");
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code char-equal} as a string.
	 *
	 * @return the function name {@code char-equal} as a string
	 */
	@Override
	protected String functionName() {
		return "CHAR-EQUAL";
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStruct#isEqualToIgnoreCase(CharacterStruct...)} as a method reference function.
	 *
	 * @return returns {@link CharacterStruct#isEqualToIgnoreCase(CharacterStruct...)} as a method reference function
	 */
	@Override
	protected Function<CharacterStruct[], Boolean> characterEqualityFunction() {
		return CharacterStruct::isEqualToIgnoreCase;
	}
}
