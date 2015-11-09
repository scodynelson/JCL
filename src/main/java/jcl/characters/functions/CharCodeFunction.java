/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Function;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

@Component
public final class CharCodeFunction extends AbstractCharacterFunction {

	private static final long serialVersionUID = -2591389262734333977L;

	private CharCodeFunction() {
		super("Returns the code attribute of character.");
	}

	@Override
	protected String functionName() {
		return "CHAR-CODE";
	}

	@Override
	protected Function<CharacterStruct, LispStruct> characterFunction() {
		return CharacterStruct::charCode;
	}
}
