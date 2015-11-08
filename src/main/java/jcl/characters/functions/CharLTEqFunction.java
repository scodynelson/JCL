/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Function;

import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

@Component
public final class CharLTEqFunction extends AbstractCharacterEqualityFunction {

	private static final long serialVersionUID = 8689782020328962711L;

	private CharLTEqFunction() {
		super("Returns true if the characters are monotonically nondecreasing; otherwise, it returns false.");
	}

	@Override
	protected String functionName() {
		return "CHAR<=";
	}

	@Override
	protected Function<CharacterStruct[], Boolean> characterEqualityFunction() {
		return CharacterStruct::isLessThanOrEqualTo;
	}
}
