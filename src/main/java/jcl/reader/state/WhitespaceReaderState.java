/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.state;

import jcl.reader.Reader;

/**
 * Step 3 of the Reader Algorithm.
 * <p>
 * If x is a whitespace[2] character, then it is discarded and step 1 is re-entered.
 * </p>
 */
final class WhitespaceReaderState implements ReaderState {

	/**
	 * Singleton instance variable.
	 */
	static final ReaderState INSTANCE = new WhitespaceReaderState();

	/**
	 * Private constructor.
	 */
	private WhitespaceReaderState() {
	}

	@Override
	public void process(final ReaderStateMediator readerStateMediator, final Reader reader, final TokenBuilder tokenBuilder) {
		readerStateMediator.read(reader, tokenBuilder);
	}
}
