/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.reader.CharacterConstants;
import jcl.reader.Reader;
import jcl.characters.CharacterStruct;
import jcl.symbols.variables.Variable;

import java.math.BigInteger;

/**
 * Implements the '#u' Lisp reader macro.
 */
public final class SharpUReaderMacroFunction extends UnicodeCharacterReaderMacroFunction {

	/**
	 * Singleton instance variable.
	 */
	public static final SharpUReaderMacroFunction INSTANCE = new SharpUReaderMacroFunction();

	/**
	 * Private constructor.
	 */
	private SharpUReaderMacroFunction() {
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_U) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_U);

		if (Variable.READ_SUPPRESS.getValue().booleanValue()) {
			return null;
		}

		final int unicodeChar = readUnicodeCharacter(reader);
		return new CharacterStruct(unicodeChar);
	}
}
