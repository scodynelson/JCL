/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.state;

import jcl.lang.LispStruct;
import jcl.lang.readtable.AttributeType;
import jcl.reader.Reader;
import jcl.lang.readtable.ReaderInputStreamStruct;
import jcl.lang.stream.ReadPeekResult;
import jcl.reader.ReaderStateMediator;
import jcl.reader.TokenBuilder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Step 5 of the Reader Algorithm.
 * <p>
 * If x is a single escape character then the next character, y, is read, or an error of type end-of-file is signaled
 * if at the end of file. y is treated as if it is a constituent whose only constituent trait is alphabetic[2]. y is
 * used to begin a token, and step 8 is entered.
 * </p>
 */
@Component
class SingleEscapeReaderState implements ReaderState {

	/**
	 * {@link ReaderStateMediator} singleton used by the reader algorithm.
	 */
	@Autowired
	private ReaderStateMediator readerStateMediator;

	@Autowired
	private Reader reader;

	@Override
	public LispStruct process(final TokenBuilder tokenBuilder) {

		final boolean isEofErrorP = tokenBuilder.isEofErrorP();
		final LispStruct eofValue = tokenBuilder.getEofValue();

		final ReaderInputStreamStruct inputStreamStruct = tokenBuilder.getInputStreamStruct();

		final ReadPeekResult readResult = reader.readChar(inputStreamStruct, isEofErrorP, eofValue, true);
		tokenBuilder.setPreviousReadResult(readResult);

		if (readResult.isEof()) {
			return readerStateMediator.readIllegalCharacter(tokenBuilder);
		}

		final int codePoint = readResult.getResult();
		tokenBuilder.addToTokenAttributes(codePoint, AttributeType.ALPHABETIC);

		return readerStateMediator.readEvenMultipleEscape(tokenBuilder);
	}
}
