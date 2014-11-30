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
		init();
	}

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	private void init() {
		ReaderVariables.READTABLE.getValue().setMacroCharacter(CharacterConstants.LEFT_PARENTHESIS, this, false);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.LEFT_PARENTHESIS;

		return readList(reader);
	}
}
