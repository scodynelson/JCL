/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Function;

import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code char-greaterp}.
 */
@Component
public final class CharGreaterpFunction extends AbstractCharacterEqualityFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -2432879529794958419L;

	/**
	 * Private constructor passing the documentation string.
	 */
	private CharGreaterpFunction() {
		super("Returns true if the characters are monotonically decreasing, ignoring differences in case; otherwise, it returns false.");
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code char-greaterp} as a string.
	 *
	 * @return the function name {@code char-greaterp} as a string
	 */
	@Override
	protected String functionName() {
		return "CHAR-GREATERP";
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStruct#isGreaterThanIgnoreCase(CharacterStruct...)} as a method reference function.
	 *
	 * @return returns {@link CharacterStruct#isGreaterThanIgnoreCase(CharacterStruct...)} as a method reference
	 * function
	 */
	@Override
	protected Function<CharacterStruct[], Boolean> characterEqualityFunction() {
		return CharacterStruct::isGreaterThanIgnoreCase;
	}
}
