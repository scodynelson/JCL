/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Predicate;

import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code standard-char-p}.
 */
@Component
public final class StandardCharPFunction extends AbstractCharacterPredicateFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 7161616010157067470L;

	/**
	 * Public constructor passing the documentation string.
	 */
	public StandardCharPFunction() {
		super("Returns true if character is a standard character; otherwise, returns false");
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code standard-char-p} as a string.
	 *
	 * @return the function name {@code standard-char-p} as a string
	 */
	@Override
	protected String functionName() {
		return "STANDARD-CHAR-P";
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
