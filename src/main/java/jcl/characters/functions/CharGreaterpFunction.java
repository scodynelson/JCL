/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Function;

import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

@Component
public final class CharGreaterpFunction extends AbstractCharacterEqualityFunction {

	private static final long serialVersionUID = -2432879529794958419L;

	private CharGreaterpFunction() {
		super("Returns true if the characters are monotonically decreasing, ignoring differences in case; otherwise, it returns false.");
	}

	@Override
	protected String functionName() {
		return "CHAR-GREATERP";
	}

	@Override
	protected Function<CharacterStruct[], Boolean> characterEqualityFunction() {
		return CharacterStruct::isGreaterThanIgnoreCase;
	}
}
