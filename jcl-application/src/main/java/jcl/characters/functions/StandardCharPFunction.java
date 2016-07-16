/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Predicate;

import jcl.lang.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code standard-char-p}.
 */
@Component
public final class StandardCharPFunction extends AbstractCharacterPredicateFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public StandardCharPFunction() {
		super("Returns true if character is a standard character; otherwise, returns false",
		      "STANDARD-CHAR-P"
		);
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStruct#isStandardChar()} as a method reference predicate.
	 *
	 * @return returns {@link CharacterStruct#isStandardChar()} as a method reference predicate
	 */
	@Override
	protected Predicate<CharacterStruct> predicate() {
		return CharacterStruct::isStandardChar;
	}
}
