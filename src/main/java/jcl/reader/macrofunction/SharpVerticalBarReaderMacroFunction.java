/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.characters.CharacterConstants;
import jcl.reader.Reader;
import jcl.reader.ReaderVariables;
import jcl.streams.ReadPeekResult;

import java.math.BigInteger;

/**
 * Implements the '#|...|#' Lisp reader macro.
 */
public final class SharpVerticalBarReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * Singleton instance variable.
	 */
	public static final SharpVerticalBarReaderMacroFunction INSTANCE = new SharpVerticalBarReaderMacroFunction();

	/**
	 * Private constructor.
	 */
	private SharpVerticalBarReaderMacroFunction() {
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.VERTICAL_LINE;

		final int baseLevel = 0;
		int level = 1;

		// NOTE: This will throw errors when it reaches an EOF
		ReadPeekResult prevReadResult = reader.readChar();
		ReadPeekResult nextReadResult = reader.readChar();

		final StringBuilder stringBuilder = new StringBuilder();
		while (true) {

			final int prevChar = prevReadResult.getResult();
			final int nextChar = nextReadResult.getResult();
			if ((prevChar == CharacterConstants.VERTICAL_LINE) && (nextChar == CharacterConstants.NUMBER_SIGN)) {
				level -= 1;
				if (level == baseLevel) {
					break;
				} else {
					stringBuilder.appendCodePoint(prevChar);
				}
			} else if ((prevChar == CharacterConstants.NUMBER_SIGN) && (nextChar == CharacterConstants.VERTICAL_LINE)) {
				stringBuilder.appendCodePoint(prevChar);
				stringBuilder.appendCodePoint(nextChar);

				// NOTE: This will throw errors when it reaches an EOF
				nextReadResult = reader.readChar();
				level += 1;
			} else {
				stringBuilder.appendCodePoint(prevChar);
			}

			// NOTE: This will throw errors when it reaches an EOF
			prevReadResult = nextReadResult;
			nextReadResult = reader.readChar();
		}

		if (ReaderVariables.READ_SUPPRESS.getValue().booleanValue()) {
			return null;
		}

		final String stringValue = stringBuilder.toString();
		return new CommentStruct(stringValue);
	}
}
