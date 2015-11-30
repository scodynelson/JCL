/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Function;

import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code char<}.
 */
@Component
public final class CharLTFunction extends AbstractCharacterEqualityFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -6833377338710486244L;

	/**
	 * Private constructor passing the documentation string.
	 */
	private CharLTFunction() {
		super("Returns true if the characters are monotonically increasing; otherwise, it returns false.");
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code char<} as a string.
	 *
	 * @return the function name {@code char<} as a string
	 */
	@Override
	protected String functionName() {
		return "CHAR<";
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStruct#isLessThan(CharacterStruct...)} as a method reference function.
	 *
	 * @return returns {@link CharacterStruct#isLessThan(CharacterStruct...)} as a method reference function
	 */
	@Override
	protected Function<CharacterStruct[], Boolean> characterEqualityFunction() {
		return CharacterStruct::isLessThan;
	}
}
