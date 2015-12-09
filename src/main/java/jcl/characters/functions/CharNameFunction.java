/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Function;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code char-name}.
 */
@Component
public final class CharNameFunction extends AbstractCharacterFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 6904342812202906067L;

	/**
	 * Public constructor passing the documentation string.
	 */
	public CharNameFunction() {
		super("Returns a string that is the name of the character, or nil if the character has no name.");
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code char-name} as a string.
	 *
	 * @return the function name {@code char-name} as a string
	 */
	@Override
	protected String functionName() {
		return "CHAR-NAME";
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStruct#charName()} as a method reference function.
	 *
	 * @return returns {@link CharacterStruct#charName()} as a method reference function
	 */
	@Override
	protected Function<CharacterStruct, LispStruct> characterFunction() {
		return CharacterStruct::charName;
	}
}
