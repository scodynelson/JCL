/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Function;

import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

@Component
public final class CharNotEqualFunction extends AbstractCharacterEqualityFunction {

	private static final long serialVersionUID = -88330282287626428L;

	private CharNotEqualFunction() {
		super("Returns true if all characters are different, ignoring differences in case; otherwise, it returns false.");
	}

	@Override
	protected String functionName() {
		return "CHAR-NOT-EQUAL";
	}

	@Override
	protected Function<CharacterStruct[], Boolean> characterEqualityFunction() {
		return CharacterStruct::isNotEqualToIgnoreCase;
	}
}
