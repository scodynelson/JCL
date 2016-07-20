/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.character;

import java.util.function.Function;
import java.util.function.Supplier;

import jcl.lang.LispStruct;
import jcl.lang.character.CharacterStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code name-char}.
 */
@Component
public final class NameCharFunction extends AbstractCharacterDesignatorFunction {

	/**
	 * Public constructor passing the documentation string.
	 */
	public NameCharFunction() {
		super("Returns the character object whose name is name. If such a character does not exist, nil is returned.",
		      "NAME-CHAR"
		);
	}

	/**
	 * {@inheritDoc}
	 * Creates a {@link Function} applying {@link LispStruct#asNamedCharacter()} against a {@link LispStruct}
	 * parameter.
	 *
	 * @return a {@link Function} applying {@link LispStruct#asNamedCharacter()} against a {@link LispStruct}
	 */
	@Override
	protected Function<LispStruct, Supplier<CharacterStruct>> characterFunction() {
		return LispStruct::asNamedCharacter;
	}
}
