/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.characters.CharacterConstants;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;

import java.math.BigInteger;

/**
 * Implements the '#r' Lisp reader macro.
 */
public final class SharpRReaderMacroFunction extends RationalReaderMacroFunction {

	/**
	 * Singleton instance variable.
	 */
	public static final SharpRReaderMacroFunction INSTANCE = new SharpRReaderMacroFunction();

	/**
	 * Private constructor.
	 */
	private SharpRReaderMacroFunction() {
		ReaderVariables.READTABLE.getValue().setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_SMALL_LETTER_R, INSTANCE);
		ReaderVariables.READTABLE.getValue().setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_CAPITAL_LETTER_R, INSTANCE);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_R) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_R);

		return readRational(reader, numArg);
	}
}
