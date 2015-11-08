/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

@Component
public final class LowerCasePFunction extends AbstractCharacterPredicateFunction {

	private static final long serialVersionUID = 142401577628870337L;

	private LowerCasePFunction() {
		super("Returns true if character is a lowercase character; otherwise, returns false.");
	}

	@Override
	protected String functionName() {
		return "LOWER-CASE-P";
	}

	@Override
	protected boolean predicateCheck(final CharacterStruct character) {
		return character.isLowerCase();
	}
}
