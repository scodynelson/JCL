/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.character;

import java.util.function.Function;

import jcl.lang.BooleanStruct;
import jcl.lang.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code char-equal}.
 */
@Component
public final class CharEqualFunction extends AbstractCharacterEqualityFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public CharEqualFunction() {
		super("Returns true if all characters are the same, ignoring differences in case; otherwise, it returns false.",
		      "CHAR-EQUAL"
		);
	}

	/**
	 * {@inheritDoc}
	 * Returns {@link CharacterStruct#isEqualToIgnoreCase(CharacterStruct...)} as a method reference predicate.
	 *
	 * @return returns {@link CharacterStruct#isEqualToIgnoreCase(CharacterStruct...)} as a method reference predicate
	 */
	@Override
	protected Function<CharacterStruct[], BooleanStruct> characterEqualityPredicate() {
		return CharacterStruct::isEqualToIgnoreCase;
	}
}
