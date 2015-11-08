/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

@Component
public final class AlphaCharPFunction extends AbstractCharacterPredicateFunction {

	private static final long serialVersionUID = 8997206745362935215L;

	private AlphaCharPFunction() {
		super("Returns true if character is an alphabetic character; otherwise, returns false.");
	}

	@Override
	protected String functionName() {
		return "ALPHA-CHAR-P";
	}

	@Override
	protected boolean predicateCheck(final CharacterStruct character) {
		return character.isAlphaChar();
	}
}
