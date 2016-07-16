/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Function;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code char-int}.
 */
@Component
public final class CharIntFunction extends AbstractCharacterFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public CharIntFunction() {
		super("Returns a non-negative integer encoding the character object.",
		      "CHAR-INT"
		);
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStruct#charInt()} as a method reference function.
	 *
	 * @return returns {@link CharacterStruct#charInt()} as a method reference function
	 */
	@Override
	protected Function<CharacterStruct, LispStruct> characterFunction() {
		return CharacterStruct::charInt;
	}
}
