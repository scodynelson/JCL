/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.state;

import jcl.LispStruct;
import jcl.characters.CharacterConstants;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.reader.TokenBuilder;
import jcl.streams.ReadPeekResult;
import org.springframework.stereotype.Component;

/**
 * Step 2 of the Reader Algorithm.
 * <p>
 * If x is an invalid character, an error of type reader-error is signaled.
 * </p>
 */
@Component
class IllegalCharacterReaderState implements ReaderState {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 6789972435089995401L;

	@Override
	public void process(final TokenBuilder tokenBuilder) {

		if (!tokenBuilder.isEofErrorP()) {
			final LispStruct eofValue = tokenBuilder.getEofValue();
			tokenBuilder.setReturnToken(eofValue);
			return;
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
	}
}
