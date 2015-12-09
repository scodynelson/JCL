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
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 8709892843954599169L;

	/**
	 * Public constructor passing the documentation string.
	 */
	public GraphicCharPFunction() {
		super("Returns true if character is a graphic character; otherwise, returns false.");
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code graphic-char-p} as a string.
	 *
	 * @return the function name {@code graphic-char-p} as a string
	 */
	@Override
	protected String functionName() {
		return "GRAPHIC-CHAR-P";
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
