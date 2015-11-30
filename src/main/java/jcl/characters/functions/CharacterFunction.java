/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Function;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.characters.CharacterStruct;
import jcl.conditions.exceptions.SimpleErrorException;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code character}.
 */
@Component
public final class CharacterFunction extends AbstractCharacterDesignatorFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -1229967753542859679L;

	/**
	 * Private constructor passing the documentation string.
	 */
	private CharacterFunction() {
		super("Returns the character denoted by the character designator.");
	}

	/**
	 * {@inheritDoc}
	 * Returns the function name {@code character} as a string.
	 *
	 * @return the function name {@code character} as a string
	 */
	@Override
	protected String functionName() {
		return "CHARACTER";
	}

	@Override
	protected Function<StringStruct, LispStruct> stringFunction() {
		return aString -> getCharacterFromString(aString.getAsJavaString(), "String");
	}

	@Override
	protected Function<SymbolStruct<?>, LispStruct> symbolFunction() {
		return symbol -> getCharacterFromString(symbol.getName(), "Symbol name");
	}

	private static CharacterStruct getCharacterFromString(final String aString, final String errorPrefix) {
		if (aString.length() != 1) {
			throw new SimpleErrorException(errorPrefix + " is not of length one: " + aString);
		}
		return new CharacterStruct(aString.charAt(0));
	}
}
