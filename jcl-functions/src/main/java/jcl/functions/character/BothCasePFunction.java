/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.character;

import java.util.function.Function;

import jcl.lang.BooleanStruct;
import jcl.lang.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code both-case-p}.
 */
@Component
public final class BothCasePFunction extends AbstractCharacterPredicateFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public BothCasePFunction() {
		super("Returns true if character is a character with case; otherwise, returns false.",
		      "BOTH-CASE-P"
		);
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStruct#isBothCase()} as a method reference predicate.
	 *
	 * @return returns {@link CharacterStruct#isBothCase()} as a method reference predicate
	 */
	@Override
	protected Function<CharacterStruct, BooleanStruct> predicate() {
		return CharacterStruct::isBothCase;
	}
}
