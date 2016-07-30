/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.character;

import java.util.function.Function;

import jcl.lang.LispStruct;
import jcl.lang.character.CharacterStructImpl;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code char-code}.
 */
@Component
public final class CharCodeFunction extends AbstractCharacterFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public CharCodeFunction() {
		super("Returns the code attribute of character.",
		      "CHAR-CODE"
		);
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStructImpl#charCode()} as a method reference function.
	 *
	 * @return returns {@link CharacterStructImpl#charCode()} as a method reference function
	 */
	@Override
	protected Function<CharacterStructImpl, LispStruct> characterFunction() {
		return CharacterStructImpl::charCode;
	}
}
