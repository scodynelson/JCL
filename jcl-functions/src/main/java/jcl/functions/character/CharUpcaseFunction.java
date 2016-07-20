/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.character;

import java.util.function.Function;

import jcl.lang.LispStruct;
import jcl.lang.character.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code char-upcase}.
 */
@Component
public final class CharUpcaseFunction extends AbstractCharacterFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public CharUpcaseFunction() {
		super("Returns the corresponding uppercase character.",
		      "CHAR-UPCASE"
		);
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
