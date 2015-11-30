/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Predicate;

import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code alphanumericp}.
 */
@Component
public final class AlphanumericpFunction extends AbstractCharacterPredicateFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 8079402123675255567L;

	/**
	 * Private constructor passing the documentation string.
	 */
	private AlphanumericpFunction() {
		super("Returns true if character is an alphabetic character or a numeric character; otherwise, returns false.");
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code alphanumericp} as a string.
	 *
	 * @return the function name {@code alphanumericp} as a string
	 */
	@Override
	protected String functionName() {
		return "ALPHANUMERICP";
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStruct#isAlphanumeric()} as a method reference predicate.
	 *
	 * @return returns {@link CharacterStruct#isAlphanumeric()} as a method reference predicate
	 */
	@Override
	protected Predicate<CharacterStruct> predicate() {
		return CharacterStruct::isAlphanumeric;
	}
}
