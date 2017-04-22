/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.character;

import java.util.function.Function;

import jcl.lang.CharacterStruct;
import jcl.lang.LispStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code char-downcase}.
 */
@Component
public final class CharDowncaseFunction extends AbstractCharacterFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public CharDowncaseFunction() {
		super("Returns the corresponding lowercase character.",
		      "CHAR-DOWNCASE"
		);
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStruct#charDowncase()} as a method reference function.
	 *
	 * @return returns {@link CharacterStruct#charDowncase()} as a method reference function
	 */
	@Override
	protected Function<CharacterStruct, LispStruct> characterFunction() {
		return CharacterStruct::charDowncase;
	}
}
