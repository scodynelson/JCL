/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Predicate;

import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code char=}.
 */
@Component
public final class CharEqFunction extends AbstractCharacterEqualityFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 6869027876359720156L;

	/**
	 * Private constructor passing the documentation string.
	 */
	private CharEqFunction() {
		super("Returns true if all characters are the same; otherwise, it returns false.");
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code char=} as a string.
	 *
	 * @return the function name {@code char=} as a string
	 */
	@Override
	protected String functionName() {
		return "CHAR=";
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStruct#isEqualTo(CharacterStruct...)} as a method reference predicate.
	 *
	 * @return returns {@link CharacterStruct#isEqualTo(CharacterStruct...)} as a method reference predicate
	 */
	@Override
	protected Predicate<CharacterStruct[]> characterEqualityPredicate() {
		return CharacterStruct::isEqualTo;
	}
}
