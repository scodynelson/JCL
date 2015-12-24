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
	 * Public constructor passing the documentation string.
	 */
	public CharacterFunction() {
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

	/**
	 * Creates a {@link Function} applying {@link #getCharacterFromString(String, String)} against a {@link
	 * StringStruct} parameter by retrieving its {@link String} value via {@link StringStruct#getAsJavaString()} and
	 * validating that it is a single character length string.
	 *
	 * @return a {@link Function} applying {@link #getCharacterFromString(String, String)} against a {@link
	 * StringStruct}
	 */
	@Override
	protected Function<StringStruct, LispStruct> stringFunction() {
		return aString -> getCharacterFromString(aString.getAsJavaString(), "String");
	}

	/**
	 * Creates a {@link Function} applying {@link #getCharacterFromString(String, String)} against a {@link
	 * SymbolStruct} parameter by retrieving its {@link String} value via {@link SymbolStruct#getName()} and validating
	 * that it is a single character length string.
	 *
	 * @return a {@link Function} applying {@link #getCharacterFromString(String, String)} against a {@link
	 * SymbolStruct}
	 */
	@Override
	protected Function<SymbolStruct, LispStruct> symbolFunction() {
		return symbol -> getCharacterFromString(symbol.getName(), "Symbol name");
	}

	/**
	 * Gets the {@link CharacterStruct} from the provided {@link String} value, ensuring the {@link String} is a single
	 * character length string.
	 *
	 * @param aString
	 * 		the {@link String} used to create the appropriate {@link CharacterStruct}
	 * @param errorPrefix
	 * 		the {@link String} error prefix for the thrown {@link SimpleErrorException} when the provided string value is
	 * 		not a single character length string
	 *
	 * @return the {@link CharacterStruct} from the provided {@link String} value
	 */
	private static CharacterStruct getCharacterFromString(final String aString, final String errorPrefix) {
		if (aString.length() != 1) {
			throw new SimpleErrorException(errorPrefix + " is not of length one: " + aString);
		}
		return CharacterStruct.valueOf(aString.charAt(0));
	}
}
