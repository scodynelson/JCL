/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Function;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code char-upcase}.
 */
@Component
public final class CharUpcaseFunction extends AbstractCharacterFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 2419349163955092011L;

	/**
	 * Private constructor passing the documentation string.
	 */
	private CharUpcaseFunction() {
		super("Returns the corresponding uppercase character.");
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code char-upcase} as a string.
	 *
	 * @return the function name {@code char-upcase} as a string
	 */
	@Override
	protected String functionName() {
		return "CHAR-UPCASE";
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStruct#toUpperCase()} as a method reference function.
	 *
	 * @return returns {@link CharacterStruct#toUpperCase()} as a method reference function
	 */
	@Override
	protected Function<CharacterStruct, LispStruct> characterFunction() {
		return CharacterStruct::toUpperCase;
	}
}
