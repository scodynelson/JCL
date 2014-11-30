/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.state;

import jcl.reader.Reader;

/**
 * Step 6 of the Reader Algorithm.
 * <p>
 * If x is a multiple escape character then a token (initially containing no characters) is begun and step 9 is
 * entered.
 * </p>
 */
final class MultipleEscapeReaderState implements ReaderState {

	/**
	 * Singleton instance variable.
	 */
	static final ReaderState INSTANCE = new MultipleEscapeReaderState();

	/**
	 * Private constructor.
	 */
	private MultipleEscapeReaderState() {
	}

	@Override
	public void process(final ReaderStateMediator readerStateMediator, final Reader reader, final TokenBuilder tokenBuilder) {
		readerStateMediator.readOddMultipleEscape(reader, tokenBuilder);
	}
}
