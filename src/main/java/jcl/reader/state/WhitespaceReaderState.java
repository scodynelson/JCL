/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.state;

import jcl.LispStruct;
import jcl.reader.ReaderStateMediator;
import jcl.reader.TokenBuilder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Step 3 of the Reader Algorithm.
 * <p>
 * If x is a whitespace[2] character, then it is discarded and step 1 is re-entered.
 * </p>
 */
@Component
class WhitespaceReaderState implements ReaderState {

	/**
	 * {@link ReaderStateMediator} singleton used by the reader algorithm.
	 */
	@Autowired
	private ReaderStateMediator readerStateMediator;

	@Override
	public LispStruct process(final TokenBuilder tokenBuilder) {
		return readerStateMediator.read(tokenBuilder);
	}
}
