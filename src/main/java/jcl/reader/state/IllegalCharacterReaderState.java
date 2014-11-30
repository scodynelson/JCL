/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.state;

import jcl.characters.CharacterConstants;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.reader.Reader;

/**
 * Step 2 of the Reader Algorithm.
 * <p>
 * If x is an invalid character, an error of type reader-error is signaled.
 * </p>
 */
final class IllegalCharacterReaderState implements ReaderState {

	/**
	 * Singleton instance variable.
	 */
	static final ReaderState INSTANCE = new IllegalCharacterReaderState();

	/**
	 * Private constructor.
	 */
	private IllegalCharacterReaderState() {
	}

	@Override
	public void process(final ReaderStateMediator readerStateMediator, final Reader reader, final TokenBuilder tokenBuilder) {
		final Integer codePoint = tokenBuilder.getPreviousReadCharacter();

		if (ReaderState.isEndOfFileCharacter(codePoint) && tokenBuilder.isEofErrorP()) {
			throw new ReaderErrorException("End-of-File was encountered.");
		} else if (codePoint != CharacterConstants.EXIT_CHAR) {
			throw new ReaderErrorException("Illegal Character was encountered: " + codePoint);
		}
	}
}
