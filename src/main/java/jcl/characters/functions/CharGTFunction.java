/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Function;

import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

@Component
public final class CharGTFunction extends AbstractCharacterEqualityFunction {

	private static final long serialVersionUID = 4682512262006296525L;

	private CharGTFunction() {
		super("Returns true if the characters are monotonically decreasing; otherwise, it returns false.");
	}

	@Override
	protected String functionName() {
		return "CHAR>";
	}

	@Override
	protected Function<CharacterStruct[], Boolean> characterEqualityFunction() {
		return CharacterStruct::isGreaterThan;
	}
}
