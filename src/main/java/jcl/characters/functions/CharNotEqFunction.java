/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Function;

import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

@Component
public final class CharNotEqFunction extends AbstractCharacterEqualityFunction {

	private static final long serialVersionUID = -4521919308468314716L;

	private CharNotEqFunction() {
		super("Returns true if all characters are different; otherwise, it returns false.");
	}

	@Override
	protected String functionName() {
		return "CHAR/=";
	}

	@Override
	protected Function<CharacterStruct[], Boolean> characterEqualityFunction() {
		return CharacterStruct::isNotEqualTo;
	}
}
