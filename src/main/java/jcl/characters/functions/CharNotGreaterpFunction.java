/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Predicate;

import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code char-not-greaterp}.
 */
@Component
public final class CharNotGreaterpFunction extends AbstractCharacterEqualityFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -5163778191524741788L;

	/**
	 * Public constructor passing the documentation string.
	 */
	public CharNotGreaterpFunction() {
		super("Returns true if the characters are monotonically nondecreasing, ignoring differences in case; otherwise, it returns false.");
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code char-not-greaterp} as a string.
	 *
	 * @return the function name {@code char-not-greaterp} as a string
	 */
	@Override
	protected String functionName() {
		return "CHAR-NOT-GREATERP";
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStruct#isLessThanOrEqualToIgnoreCase(CharacterStruct...)} as a method reference
	 * function.
	 *
	 * @return returns {@link CharacterStruct#isLessThanOrEqualToIgnoreCase(CharacterStruct...)} as a method reference
	 * function
	 */
	@Override
	protected Predicate<CharacterStruct[]> characterEqualityPredicate() {
		return CharacterStruct::isLessThanOrEqualToIgnoreCase;
	}
}
