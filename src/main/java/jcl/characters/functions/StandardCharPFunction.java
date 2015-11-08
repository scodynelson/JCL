/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

@Component
public final class StandardCharPFunction extends AbstractCharacterPredicateFunction {

	private static final long serialVersionUID = 7161616010157067470L;

	private StandardCharPFunction() {
		super("Returns true if character is a standard character; otherwise, returns false");
	}

	@Override
	protected String functionName() {
		return "STANDARD-CHAR-P";
	}

	@Override
	protected boolean predicateCheck(final CharacterStruct character) {
		return character.isStandardChar();
	}
}
