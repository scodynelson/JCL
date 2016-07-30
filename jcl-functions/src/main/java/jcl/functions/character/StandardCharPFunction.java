/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.character;

import java.util.function.Predicate;

import jcl.lang.character.CharacterStructImpl;
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
	 * Returns {@link CharacterStructImpl#isStandardChar()} as a method reference predicate.
	 *
	 * @return returns {@link CharacterStructImpl#isStandardChar()} as a method reference predicate
	 */
	@Override
	protected Predicate<CharacterStructImpl> predicate() {
		return CharacterStructImpl::isStandardChar;
	}
}
