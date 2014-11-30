/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.characters.CharacterConstants;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
import jcl.streams.ReadPeekResult;

import java.math.BigInteger;

/**
 * Implements the ';' Lisp reader macro.
 */
public final class SemicolonReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * Singleton instance variable.
	 */
	public static final SemicolonReaderMacroFunction INSTANCE = new SemicolonReaderMacroFunction();

	/**
	 * Private constructor.
	 */
	private SemicolonReaderMacroFunction() {
		init();
	}

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	private void init() {
		ReaderVariables.READTABLE.getValue().setMacroCharacter(CharacterConstants.SEMICOLON, this, false);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.SEMICOLON;

		final StringBuilder stringBuilder = new StringBuilder();

		ReadPeekResult readResult = reader.readChar(false, null, false);
		Integer readChar = readResult.getResult();
		while (!readResult.isEof() && (readChar.intValue() != CharacterConstants.NEWLINE)) {
			stringBuilder.appendCodePoint(readChar);

			readResult = reader.readChar(false, null, false);
			readChar = readResult.getResult();
		}

		if (ReaderVariables.READ_SUPPRESS.getValue().booleanValue()) {
			return null;
		}

		final String stringValue = stringBuilder.toString();
		return new CommentStruct(stringValue);
	}
}
