/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Function;

import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

@Component
public final class CharNotGreaterpFunction extends AbstractCharacterEqualityFunction {

	private static final long serialVersionUID = -5163778191524741788L;

	private CharNotGreaterpFunction() {
		super("Returns true if the characters are monotonically nondecreasing, ignoring differences in case; otherwise, it returns false.");
	}

	@Override
	protected String functionName() {
		return "CHAR-NOT-GREATERP";
	}

	@Override
	protected Function<CharacterStruct[], Boolean> characterEqualityFunction() {
		return CharacterStruct::isLessThanOrEqualToIgnoreCase;
	}
}
