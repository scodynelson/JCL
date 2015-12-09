/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Predicate;

import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code lower-case-p}.
 */
@Component
public final class LowerCasePFunction extends AbstractCharacterPredicateFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 142401577628870337L;

	/**
	 * Public constructor passing the documentation string.
	 */
	public LowerCasePFunction() {
		super("Returns true if character is a lowercase character; otherwise, returns false.");
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code lower-case-p} as a string.
	 *
	 * @return the function name {@code lower-case-p} as a string
	 */
	@Override
	protected String functionName() {
		return "LOWER-CASE-P";
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStruct#isLowerCase()} as a method reference predicate.
	 *
	 * @return returns {@link CharacterStruct#isLowerCase()} as a method reference predicate
	 */
	@Override
	protected Predicate<CharacterStruct> predicate() {
		return CharacterStruct::isLowerCase;
	}
}
