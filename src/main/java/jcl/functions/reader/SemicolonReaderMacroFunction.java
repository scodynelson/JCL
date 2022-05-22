/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import jcl.lang.InputStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ReadCharResult;
import jcl.reader.CommentStruct;
import jcl.util.CodePointConstants;

/**
 * Implements the ';' Lisp reader macro.
 */
public final class SemicolonReaderMacroFunction extends ReaderMacroFunctionImpl {

	public SemicolonReaderMacroFunction() {
		super("SEMICOLON");
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint,
	                            final IntegerStruct numberArgument) {
		assert codePoint == CodePointConstants.SEMICOLON;

		final StringBuilder stringBuilder = new StringBuilder();

		ReadCharResult readResult = inputStreamStruct.readChar(false, null);
		Integer nextCodePoint = readResult.getResult();
		while (!readResult.isEof() && (nextCodePoint.intValue() != CodePointConstants.NEWLINE)) {
			stringBuilder.appendCodePoint(nextCodePoint);

			readResult = inputStreamStruct.readChar(false, null);
			nextCodePoint = readResult.getResult();
		}

		final String stringValue = stringBuilder.toString();
		return new CommentStruct(stringValue);
	}
}
