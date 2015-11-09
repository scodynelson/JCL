/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Predicate;

import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

@Component
public final class BothCasePFunction extends AbstractCharacterPredicateFunction {

	private static final long serialVersionUID = 1586747610940151180L;

	private BothCasePFunction() {
		super("Returns true if character is a character with case; otherwise, returns false.");
	}

	@Override
	protected String functionName() {
		return "BOTH-CASE-P";
	}

	@Override
	protected Predicate<CharacterStruct> predicate() {
		return CharacterStruct::isBothCase;
	}
}
