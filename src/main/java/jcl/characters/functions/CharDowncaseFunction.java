/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Function;

import jcl.LispStruct;
import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

@Component
public final class CharDowncaseFunction extends AbstractCharacterFunction {

	private static final long serialVersionUID = 9125259122346104648L;

	private CharDowncaseFunction() {
		super("Returns the corresponding lowercase character.");
	}

	@Override
	protected String functionName() {
		return "CHAR-DOWNCASE";
	}

	@Override
	protected Function<CharacterStruct, LispStruct> characterFunction() {
		return CharacterStruct::toLowerCase;
	}
}
