/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.reader.CharacterConstants;
import jcl.reader.Reader;

import java.math.BigInteger;

/**
 * Implements the '(...)' Lisp reader macro.
 */
public final class LeftParenthesisReaderMacroFunction extends ListReaderMacroFunction {

	/**
	 * Singleton instance variable.
	 */
	public static final LeftParenthesisReaderMacroFunction INSTANCE = new LeftParenthesisReaderMacroFunction();

	/**
	 * Private constructor.
	 */
	private LeftParenthesisReaderMacroFunction() {
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.LEFT_PARENTHESIS;

		return readList(reader);
	}
}