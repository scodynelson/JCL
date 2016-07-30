/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.character;

import java.util.function.Function;

import jcl.lang.LispStruct;
import jcl.lang.character.CharacterStructImpl;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code char-name}.
 */
@Component
public final class CharNameFunction extends AbstractCharacterFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public CharNameFunction() {
		super("Returns a string that is the name of the character, or nil if the character has no name.",
		      "CHAR-NAME"
		);
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStructImpl#charName()} as a method reference function.
	 *
	 * @return returns {@link CharacterStructImpl#charName()} as a method reference function
	 */
	@Override
	protected Function<CharacterStructImpl, LispStruct> characterFunction() {
		return CharacterStructImpl::charName;
	}
}
