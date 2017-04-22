/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.character;

import java.util.function.Function;

import jcl.lang.BooleanStruct;
import jcl.lang.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code char-greaterp}.
 */
@Component
public final class CharGreaterpFunction extends AbstractCharacterEqualityFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public CharGreaterpFunction() {
		super("Returns true if the characters are monotonically decreasing, ignoring differences in case; otherwise, it returns false.",
		      "CHAR-GREATERP"
		);
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStruct#isGreaterThanIgnoreCase(CharacterStruct...)} as a method reference predicate.
	 *
	 * @return returns {@link CharacterStruct#isGreaterThanIgnoreCase(CharacterStruct...)} as a method reference
	 * predicate
	 */
	@Override
	protected Function<CharacterStruct[], BooleanStruct> characterEqualityPredicate() {
		return CharacterStruct::isGreaterThanIgnoreCase;
	}
}
