/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.reader.CharacterConstants;
import jcl.reader.Reader;
import jcl.structs.streams.ReadPeekResult;
import jcl.structs.symbols.variables.Variable;

import java.math.BigInteger;

/**
 * Implements the ';' Lisp reader macro.
 */
public final class SemicolonReaderMacroFunction extends ReaderMacroFunction {

	/**
	 * Singleton instance variable.
	 */
	public static final SemicolonReaderMacroFunction INSTANCE = new SemicolonReaderMacroFunction();

	/**
	 * Private constructor.
	 */
	private SemicolonReaderMacroFunction() {
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

		if (Variable.READ_SUPPRESS.getValue().booleanValue()) {
			return null;
		}

		final String stringValue = stringBuilder.toString();
		return new CommentStruct(stringValue);
	}
}
