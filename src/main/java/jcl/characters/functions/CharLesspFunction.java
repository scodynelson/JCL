/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Function;

import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

@Component
public final class CharLesspFunction extends AbstractCharacterEqualityFunction {

	private static final long serialVersionUID = -8873603818879068098L;

	private CharLesspFunction() {
		super("Returns true if the characters are monotonically increasing, ignoring differences in case; otherwise, it returns false.");
	}

	@Override
	protected String functionName() {
		return "CHAR-LESSP";
	}

	@Override
	protected Function<CharacterStruct[], Boolean> characterEqualityFunction() {
		return CharacterStruct::isLessThanIgnoreCase;
	}
}
