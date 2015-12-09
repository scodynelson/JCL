/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Predicate;

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
	 * Public constructor passing the documentation string.
	 */
	public CharLTEqFunction() {
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
	 * Returns {@link CharacterStruct#isLessThanOrEqualTo(CharacterStruct...)} as a method reference predicate.
	 *
	 * @return returns {@link CharacterStruct#isLessThanOrEqualTo(CharacterStruct...)} as a method reference predicate
	 */
	@Override
	protected Predicate<CharacterStruct[]> characterEqualityPredicate() {
		return CharacterStruct::isLessThanOrEqualTo;
	}
}
