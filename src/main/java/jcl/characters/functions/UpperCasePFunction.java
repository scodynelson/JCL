/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Predicate;

import jcl.characters.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code upper-case-p}.
 */
@Component
public final class UpperCasePFunction extends AbstractCharacterPredicateFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -6043849777082339800L;

	/**
	 * Public constructor passing the documentation string.
	 */
	public UpperCasePFunction() {
		super("Returns true if character is an uppercase character; otherwise, returns false.");
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code upper-case-p} as a string.
	 *
	 * @return the function name {@code upper-case-p} as a string
	 */
	@Override
	protected String functionName() {
		return "UPPER-CASE-P";
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStruct#isUpperCase()} as a method reference predicate.
	 *
	 * @return returns {@link CharacterStruct#isUpperCase()} as a method reference predicate
	 */
	@Override
	protected Predicate<CharacterStruct> predicate() {
		return CharacterStruct::isUpperCase;
	}
}
