/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Function;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code char-downcase}.
 */
@Component
public final class CharDowncaseFunction extends AbstractCharacterFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 9125259122346104648L;

	/**
	 * Private constructor passing the documentation string.
	 */
	private CharDowncaseFunction() {
		super("Returns the corresponding lowercase character.");
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code char-downcase} as a string.
	 *
	 * @return the function name {@code char-downcase} as a string
	 */
	@Override
	protected String functionName() {
		return "CHAR-DOWNCASE";
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStruct#toLowerCase()} as a method reference function.
	 *
	 * @return returns {@link CharacterStruct#toLowerCase()} as a method reference function
	 */
	@Override
	protected Function<CharacterStruct, LispStruct> characterFunction() {
		return CharacterStruct::toLowerCase;
	}
}
