/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Function;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code char-code}.
 */
@Component
public final class CharCodeFunction extends AbstractCharacterFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -2591389262734333977L;

	/**
	 * Public constructor passing the documentation string.
	 */
	public CharCodeFunction() {
		super("Returns the code attribute of character.");
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code char-code} as a string.
	 *
	 * @return the function name {@code char-code} as a string
	 */
	@Override
	protected String functionName() {
		return "CHAR-CODE";
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStruct#charCode()} as a method reference function.
	 *
	 * @return returns {@link CharacterStruct#charCode()} as a method reference function
	 */
	@Override
	protected Function<CharacterStruct, LispStruct> characterFunction() {
		return CharacterStruct::charCode;
	}
}
