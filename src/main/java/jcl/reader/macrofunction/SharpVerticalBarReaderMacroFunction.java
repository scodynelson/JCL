/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.math.BigInteger;
import java.util.Optional;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.characters.CharacterConstants;
import jcl.reader.Reader;
import jcl.reader.ReaderMacroFunction;
import jcl.reader.struct.ReaderVariables;
import jcl.streams.ReadPeekResult;
import jcl.symbols.NILStruct;
import org.springframework.stereotype.Component;

/**
 * Implements the '#|...|#' Lisp reader macro.
 */
@Component
public class SharpVerticalBarReaderMacroFunction extends ReaderMacroFunction {

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		ReaderVariables.READTABLE.getVariableValue().setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.VERTICAL_LINE, this);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Optional<BigInteger> numberArgument) {
		assert codePoint == CharacterConstants.VERTICAL_LINE;

		final int baseLevel = 0;
		int currentLevel = 1;

		// NOTE: This will throw errors when it reaches an EOF
		ReadPeekResult previousReadResult = reader.readChar(true, NILStruct.INSTANCE, false);
		ReadPeekResult nextReadResult = reader.readChar(true, NILStruct.INSTANCE, false);

		final StringBuilder stringBuilder = new StringBuilder();
		while (true) {

			final int previousCodePoint = previousReadResult.getResult();
			final int nextCodePoint = nextReadResult.getResult();
			if ((previousCodePoint == CharacterConstants.VERTICAL_LINE) && (nextCodePoint == CharacterConstants.NUMBER_SIGN)) {
				currentLevel -= 1;
				if (currentLevel == baseLevel) {
					break;
				} else {
					stringBuilder.appendCodePoint(previousCodePoint);
				}
			} else if ((previousCodePoint == CharacterConstants.NUMBER_SIGN) && (nextCodePoint == CharacterConstants.VERTICAL_LINE)) {
				stringBuilder.appendCodePoint(previousCodePoint);
				stringBuilder.appendCodePoint(nextCodePoint);

				// NOTE: This will throw errors when it reaches an EOF
				nextReadResult = reader.readChar(true, NILStruct.INSTANCE, false);
				currentLevel += 1;
			} else {
				stringBuilder.appendCodePoint(previousCodePoint);
			}

			// NOTE: This will throw errors when it reaches an EOF
			previousReadResult = nextReadResult;
			nextReadResult = reader.readChar(true, NILStruct.INSTANCE, false);
		}

//		final String stringValue = stringBuilder.toString();
//		return new CommentStruct(stringValue);
		return NILStruct.INSTANCE;
	}
}
