/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Predicate;

import jcl.characters.CharacterStruct;
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
	protected Predicate<CharacterStruct> predicate() {
		return CharacterStruct::isGraphicChar;
	}
}
