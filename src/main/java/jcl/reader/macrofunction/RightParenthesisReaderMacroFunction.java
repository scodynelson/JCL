/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.characters.CharacterConstants;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;

import java.math.BigInteger;

/**
 * Implements the ')' Lisp reader macro.
 */
public final class RightParenthesisReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * Singleton instance variable.
	 */
	public static final RightParenthesisReaderMacroFunction INSTANCE = new RightParenthesisReaderMacroFunction();

	/**
	 * Private constructor.
	 */
	private RightParenthesisReaderMacroFunction() {
		ReaderVariables.READTABLE.getValue().setMacroCharacter(CharacterConstants.RIGHT_PARENTHESIS, INSTANCE, false);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.RIGHT_PARENTHESIS;

		throw new ReaderErrorException("Unmatched close parenthesis.");
	}
}
