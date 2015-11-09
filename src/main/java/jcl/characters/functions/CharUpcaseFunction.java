/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Function;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

@Component
public final class CharUpcaseFunction extends AbstractCharacterFunction {

	private static final long serialVersionUID = 2419349163955092011L;

	private CharUpcaseFunction() {
		super("Returns the corresponding uppercase character.");
	}

	@Override
	protected String functionName() {
		return "CHAR-UPCASE";
	}

	@Override
	protected Function<CharacterStruct, LispStruct> characterFunction() {
		return CharacterStruct::toUpperCase;
	}
}
