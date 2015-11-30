/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Function;

import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code char<=}.
 */
@Component
public final class CharLTEqFunction extends AbstractCharacterEqualityFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 8689782020328962711L;

	/**
	 * Private constructor passing the documentation string.
	 */
	private CharLTEqFunction() {
		super("Returns true if the characters are monotonically nondecreasing; otherwise, it returns false.");
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code char<=} as a string.
	 *
	 * @return the function name {@code char<=} as a string
	 */
	@Override
	protected String functionName() {
		return "CHAR<=";
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStruct#isLessThanOrEqualTo(CharacterStruct...)} as a method reference function.
	 *
	 * @return returns {@link CharacterStruct#isLessThanOrEqualTo(CharacterStruct...)} as a method reference function
	 */
	@Override
	protected Function<CharacterStruct[], Boolean> characterEqualityFunction() {
		return CharacterStruct::isLessThanOrEqualTo;
	}
}
