/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Function;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

@Component
public final class CharIntFunction extends AbstractCharacterFunction {

	private static final long serialVersionUID = -3248960877271323642L;

	private CharIntFunction() {
		super("Returns a non-negative integer encoding the character object.");
	}

	@Override
	protected String functionName() {
		return "CHAR-INT";
	}

	@Override
	protected Function<CharacterStruct, LispStruct> characterFunction() {
		return CharacterStruct::charInt;
	}
}
