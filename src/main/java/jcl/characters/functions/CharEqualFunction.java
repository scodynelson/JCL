/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Function;

import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

@Component
public final class CharEqualFunction extends AbstractCharacterEqualityFunction {

	private static final long serialVersionUID = 1671077613128290225L;

	private CharEqualFunction() {
		super("Returns true if all characters are the same, ignoring differences in case; otherwise, it returns false.");
	}

	@Override
	protected String functionName() {
		return "CHAR-EQUAL";
	}

	@Override
	protected Function<CharacterStruct[], Boolean> characterEqualityFunction() {
		return CharacterStruct::isEqualToIgnoreCase;
	}
}
