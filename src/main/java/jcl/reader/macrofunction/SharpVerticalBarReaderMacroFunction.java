/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.characters.CharacterConstants;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
import jcl.streams.ReadPeekResult;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.math.BigInteger;

/**
 * Implements the '#|...|#' Lisp reader macro.
 */
@Component
public class SharpVerticalBarReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		ReaderVariables.READTABLE.getValue().setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.VERTICAL_LINE, this);
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