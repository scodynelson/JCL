/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Predicate;

import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

@Component
public final class GraphicCharPFunction extends AbstractCharacterPredicateFunction {

	private static final long serialVersionUID = 8709892843954599169L;

	private GraphicCharPFunction() {
		super("Returns true if character is a graphic character; otherwise, returns false.");
	}

	@Override
	protected String functionName() {
		return "GRAPHIC-CHAR-P";
	}

	@Override
	protected Predicate<CharacterStruct> predicate() {
		return CharacterStruct::isGraphicChar;
	}
}
