/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.state;

import jcl.lang.LispStruct;
import jcl.reader.ReaderStateMediator;
import jcl.reader.TokenBuilder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Step 6 of the Reader Algorithm.
 * <p>
 * If x is a multiple escape character then a token (initially containing no characters) is begun and step 9 is
 * entered.
 * </p>
 */
@Component
class MultipleEscapeReaderState implements ReaderState {

	/**
	 * {@link ReaderStateMediator} singleton used by the reader algorithm.
	 */
	@Autowired
	private ReaderStateMediator readerStateMediator;

	@Override
	public LispStruct process(final TokenBuilder tokenBuilder) {
		return readerStateMediator.readOddMultipleEscape(tokenBuilder);
	}
}
