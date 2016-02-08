/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Function;

import jcl.LispStruct;
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
		super("Returns the character object whose name is name. If such a character does not exist, nil is returned.");
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code name-char} as a string.
	 *
	 * @return the function name {@code name-char} as a string
	 */
	@Override
	protected String functionName() {
		return "NAME-CHAR";
	}

	/**
	 * {@inheritDoc}
	 * Creates a {@link Function} applying {@link LispStruct#toNamedCharacter()} against a {@link LispStruct}
	 * parameter.
	 *
	 * @return a {@link Function} applying {@link LispStruct#toNamedCharacter()} against a {@link LispStruct}
	 */
	@Override
	protected Function<LispStruct, LispStruct> characterFunction() {
		return LispStruct::toNamedCharacter;
	}
}
