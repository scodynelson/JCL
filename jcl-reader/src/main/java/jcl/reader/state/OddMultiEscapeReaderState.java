/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.state;

import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.readtable.AttributeType;
import jcl.lang.readtable.SyntaxType;
import jcl.lang.statics.ReaderVariables;
import jcl.lang.stream.ReadPeekResult;
import jcl.reader.ReaderStateMediator;
import jcl.reader.TokenBuilder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Step 9 of the Reader Algorithm.
 * <p>
 * At this point a token is being accumulated, and an odd number of multiple escape characters have been encountered.
 * If at end of file, an error of type end-of-file is signaled. Otherwise, a character, y, is read, and one of the
 * following actions is performed according to its syntax type:
 * <p>
 * If y is a constituent, macro, or whitespace character, y is treated as a constituent whose only constituent trait is
 * alphabetic. Y is appended to the token being built, and step 9 is repeated.
 * </p>
 * <p>
 * If y is a single escape character, then the next character, z, is read, or an error of type end-of-file is signaled
 * if at end of file. Z is treated as a constituent whose only constituent trait is alphabetic. Z is appended to the
 * token being built, and step 9 is repeated.
 * </p>
 * <p>
 * If y is a multiple escape character, then step 8 is entered.
 * </p>
 * <p>
 * If y is an invalid character, an error of type reader-error is signaled.
 * </p>
 */
//@Component
class OddMultiEscapeReaderState implements ReaderState {

	/**
	 * {@link ReaderStateMediator} singleton used by the reader algorithm.
	 */
	@Autowired
	private ReaderStateMediator readerStateMediator;

	@Override
	public LispStruct process(final TokenBuilder tokenBuilder) {

		final boolean isEofErrorP = tokenBuilder.isEofErrorP();
		final LispStruct eofValue = tokenBuilder.getEofValue();

		final InputStreamStruct inputStreamStruct = tokenBuilder.getInputStreamStruct();

		ReadPeekResult readResult = inputStreamStruct.readChar(isEofErrorP, eofValue, true);
		tokenBuilder.setPreviousReadResult(readResult);

		if (readResult.isEof()) {
			return readerStateMediator.readIllegalCharacter(tokenBuilder);
		}

		int codePoint = readResult.getResult();

		final ReadtableStruct readtable = ReaderVariables.READTABLE.getVariableValue();
		final SyntaxType syntaxType = readtable.getSyntaxType(codePoint);

		if ((syntaxType == SyntaxType.CONSTITUENT)
				|| (syntaxType == SyntaxType.WHITESPACE)
				|| (syntaxType == SyntaxType.TERMINATING)
				|| (syntaxType == SyntaxType.NON_TERMINATING)) {

			tokenBuilder.addToTokenAttributes(codePoint, AttributeType.ALPHABETIC);

			return readerStateMediator.readOddMultipleEscape(tokenBuilder);
		} else if (syntaxType == SyntaxType.SINGLE_ESCAPE) {

			readResult = inputStreamStruct.readChar(isEofErrorP, eofValue, true);
			tokenBuilder.setPreviousReadResult(readResult);

			if (readResult.isEof()) {
				return readerStateMediator.readIllegalCharacter(tokenBuilder);
			}

			codePoint = readResult.getResult();
			tokenBuilder.addToTokenAttributes(codePoint, AttributeType.ALPHABETIC);

			return readerStateMediator.readOddMultipleEscape(tokenBuilder);
		} else if (syntaxType == SyntaxType.MULTIPLE_ESCAPE) {
			tokenBuilder.setMultiEscapedToken();
			return readerStateMediator.readEvenMultipleEscape(tokenBuilder);
		} else {
			return readerStateMediator.readIllegalCharacter(tokenBuilder);
		}
	}
}
