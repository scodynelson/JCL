/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Function;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

@Component
public final class CharNameFunction extends AbstractCharacterFunction {

	private static final long serialVersionUID = 6904342812202906067L;

	private CharNameFunction() {
		super("Returns a string that is the name of the character, or nil if the character has no name.");
	}

	@Override
	protected String functionName() {
		return "CHAR-NAME";
	}

	@Override
	protected Function<CharacterStruct, LispStruct> characterFunction() {
		return CharacterStruct::charName;
	}
}
