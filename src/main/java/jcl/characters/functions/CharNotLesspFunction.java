/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Function;

import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

@Component
public final class CharNotLesspFunction extends AbstractCharacterEqualityFunction {

	private static final long serialVersionUID = -302980532887992947L;

	private CharNotLesspFunction() {
		super("Returns true if the characters are monotonically nonincreasing, ignoring differences in case; otherwise, it returns false.");
	}

	@Override
	protected String functionName() {
		return "CHAR-NOT-LESSP";
	}

	@Override
	protected Function<CharacterStruct[], Boolean> characterEqualityFunction() {
		return CharacterStruct::isGreaterThanIgnoreCase;
	}
}
