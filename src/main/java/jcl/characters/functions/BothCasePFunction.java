/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Predicate;

import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code both-case-p}.
 */
@Component
public final class BothCasePFunction extends AbstractCharacterPredicateFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 1586747610940151180L;

	/**
	 * Private constructor passing the documentation string.
	 */
	private BothCasePFunction() {
		super("Returns true if character is a character with case; otherwise, returns false.");
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code both-case-p} as a string.
	 *
	 * @return the function name {@code both-case-p} as a string
	 */
	@Override
	protected String functionName() {
		return "BOTH-CASE-P";
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStruct#isBothCase()} as a method reference predicate.
	 *
	 * @return returns {@link CharacterStruct#isBothCase()} as a method reference predicate
	 */
	@Override
	protected Predicate<CharacterStruct> predicate() {
		return CharacterStruct::isBothCase;
	}
}
