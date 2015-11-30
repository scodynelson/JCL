/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters.functions;

import java.util.function.Function;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.characters.CharacterStruct;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

/**
 * Function implementation for {@code name-char}.
 */
@Component
public final class NameCharFunction extends AbstractCharacterDesignatorFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 3406210294951003426L;

	/**
	 * Private constructor passing the documentation string.
	 */
	private NameCharFunction() {
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

	@Override
	protected Function<StringStruct, LispStruct> stringFunction() {
		return aString -> CharacterStruct.nameChar(aString.getAsJavaString());
	}

	@Override
	protected Function<SymbolStruct<?>, LispStruct> symbolFunction() {
		return symbol -> CharacterStruct.nameChar(symbol.getName());
	}
}
