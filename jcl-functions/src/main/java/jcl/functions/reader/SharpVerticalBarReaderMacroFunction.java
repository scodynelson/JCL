/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import java.math.BigInteger;
import java.util.Optional;

import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.reader.Reader;
import jcl.lang.readtable.ReaderInputStreamStruct;
import jcl.lang.statics.ReaderVariables;
import jcl.lang.stream.ReadPeekResult;
import jcl.util.CodePointConstants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.DependsOn;
import org.springframework.stereotype.Component;

/**
 * Implements the '#|...|#' Lisp reader macro.
 */
@Component
@DependsOn("readerBootstrap")
public class SharpVerticalBarReaderMacroFunction extends ReaderMacroFunctionImpl {

	private final Reader reader;

	@Autowired
	public SharpVerticalBarReaderMacroFunction(final Reader reader) {
		this.reader = reader;
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
		ReaderVariables.READTABLE.getVariableValue().setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.VERTICAL_LINE, this);
	}

	@Override
	public LispStruct readMacro(final ReaderInputStreamStruct inputStreamStruct, final int codePoint, final Optional<BigInteger> numberArgument) {
		assert codePoint == CodePointConstants.VERTICAL_LINE;

		final int baseLevel = 0;
		int currentLevel = 1;

		// NOTE: This will throw errors when it reaches an EOF
		ReadPeekResult previousReadResult = reader.readChar(inputStreamStruct, true, NILStruct.INSTANCE, false);
		ReadPeekResult nextReadResult = reader.readChar(inputStreamStruct,true, NILStruct.INSTANCE, false);

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
				nextReadResult = reader.readChar(inputStreamStruct,true, NILStruct.INSTANCE, false);
				currentLevel += 1;
			} else {
				stringBuilder.appendCodePoint(previousCodePoint);
			}

			// NOTE: This will throw errors when it reaches an EOF
			previousReadResult = nextReadResult;
			nextReadResult = reader.readChar(inputStreamStruct,true, NILStruct.INSTANCE, false);
		}

//		final String stringValue = stringBuilder.toString();
//		return new CommentStruct(stringValue);
		return NILStruct.INSTANCE;
	}
}
