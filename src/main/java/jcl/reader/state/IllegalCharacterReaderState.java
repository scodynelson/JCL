/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.state;

import jcl.LispStruct;
import jcl.characters.CharacterConstants;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.reader.TokenBuilder;
import jcl.streams.ReadPeekResult;
import jcl.symbols.NILStruct;
import org.springframework.stereotype.Component;

/**
 * Step 2 of the Reader Algorithm.
 * <p>
 * If x is an invalid character, an error of type reader-error is signaled.
 * </p>
 */
@Component
class IllegalCharacterReaderState implements ReaderState {

	@Override
	public LispStruct process(final TokenBuilder tokenBuilder) {

		if (!tokenBuilder.isEofErrorP()) {
			return tokenBuilder.getEofValue();
		}

		final ReadPeekResult readResult = tokenBuilder.getPreviousReadResult();

		if (readResult == null) {
			throw new ReaderErrorException("No token elements could be read.");
		}

		if (readResult.isEof()) {
			throw new ReaderErrorException("End-of-File was encountered.");
		}

		final int codePoint = readResult.getResult();
		if (codePoint != CharacterConstants.EXIT_CHAR) {
			throw new ReaderErrorException("Illegal Character was encountered: " + codePoint);
		}

		return NILStruct.INSTANCE;
	}
}
