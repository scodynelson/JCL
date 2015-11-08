/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Function;

import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

@Component
public final class CharEqFunction extends AbstractCharacterEqualityFunction {

	private static final long serialVersionUID = 6869027876359720156L;

	private CharEqFunction() {
		super("Returns true if all characters are the same; otherwise, it returns false.");
	}

	@Override
	protected String functionName() {
		return "CHAR=";
	}

	@Override
	protected Function<CharacterStruct[], Boolean> characterEqualityFunction() {
		return CharacterStruct::isEqualTo;
	}
}
