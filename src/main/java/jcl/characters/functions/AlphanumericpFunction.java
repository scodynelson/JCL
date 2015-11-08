/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

@Component
public final class AlphanumericpFunction extends AbstractCharacterPredicateFunction {

	private static final long serialVersionUID = 8079402123675255567L;

	private AlphanumericpFunction() {
		super("Returns true if character is an alphabetic character or a numeric character; otherwise, returns false.");
	}

	@Override
	protected String functionName() {
		return "ALPHANUMERICP";
	}

	@Override
	protected boolean predicateCheck(final CharacterStruct character) {
		return character.isAlphanumeric();
	}
}
