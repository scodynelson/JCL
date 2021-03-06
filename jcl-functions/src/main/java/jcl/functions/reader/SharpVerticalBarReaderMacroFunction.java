/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import jcl.lang.InputStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.ReadCharResult;
import jcl.reader.CommentStruct;
import jcl.util.CodePointConstants;

/**
 * Implements the '#|...|#' Lisp reader macro.
 */
public final class SharpVerticalBarReaderMacroFunction extends ReaderMacroFunctionImpl {

	public SharpVerticalBarReaderMacroFunction() {
		super("SHARP-VERTICAL-BAR");
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint,
	                            final IntegerStruct numberArgument) {
		assert codePoint == CodePointConstants.VERTICAL_LINE;

		final int baseLevel = 0;
		int currentLevel = 1;

		// NOTE: This will throw errors when it reaches an EOF
		ReadCharResult previousReadResult = inputStreamStruct.readChar(true, NILStruct.INSTANCE);
		ReadCharResult nextReadResult = inputStreamStruct.readChar(true, NILStruct.INSTANCE);

		final StringBuilder stringBuilder = new StringBuilder();
		while (true) {

			final int previousCodePoint = previousReadResult.getResult();
			final int nextCodePoint = nextReadResult.getResult();
			if ((previousCodePoint == CodePointConstants.VERTICAL_LINE) && (nextCodePoint == CodePointConstants.NUMBER_SIGN)) {
				currentLevel -= 1;
				if (currentLevel == baseLevel) {
					break;
				} else {
					stringBuilder.appendCodePoint(previousCodePoint);
				}
			} else if ((previousCodePoint == CodePointConstants.NUMBER_SIGN) && (nextCodePoint == CodePointConstants.VERTICAL_LINE)) {
				stringBuilder.appendCodePoint(previousCodePoint);
				stringBuilder.appendCodePoint(nextCodePoint);

				// NOTE: This will throw errors when it reaches an EOF
				nextReadResult = inputStreamStruct.readChar(true, NILStruct.INSTANCE);
				currentLevel += 1;
			} else {
				stringBuilder.appendCodePoint(previousCodePoint);
			}

			// NOTE: This will throw errors when it reaches an EOF
			previousReadResult = nextReadResult;
			nextReadResult = inputStreamStruct.readChar(true, NILStruct.INSTANCE);
		}

		final String stringValue = stringBuilder.toString();
		return new CommentStruct(stringValue);
	}
}
