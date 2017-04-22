/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.character;

import java.util.function.Function;

import jcl.lang.BooleanStruct;
import jcl.lang.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code graphic-char-p}.
 */
@Component
public final class GraphicCharPFunction extends AbstractCharacterPredicateFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public GraphicCharPFunction() {
		super("Returns true if character is a graphic character; otherwise, returns false.",
		      "GRAPHIC-CHAR-P"
		);
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStruct#isGraphicChar()} as a method reference predicate.
	 *
	 * @return returns {@link CharacterStruct#isGraphicChar()} as a method reference predicate
	 */
	@Override
	protected Function<CharacterStruct, BooleanStruct> predicate() {
		return CharacterStruct::isGraphicChar;
	}
}
