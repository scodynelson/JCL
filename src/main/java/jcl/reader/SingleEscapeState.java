/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader;

import jcl.LispStruct;
import jcl.structs.streams.ReadResult;

/**
 * Step 5 of the Reader Algorithm.
 * <p>
 * If x is a single escape character then the next character, y, is read, or an error of type end-of-file is signaled
 * if at the end of file. y is treated as if it is a constituent whose only constituent trait is alphabetic[2]. y is
 * used to begin a token, and step 8 is entered.
 * </p>
 */
final class SingleEscapeState implements State {

	/**
	 * Singleton instance variable.
	 */
	static final State INSTANCE = new SingleEscapeState();

	/**
	 * Private constructor.
	 */
	private SingleEscapeState() {
	}

	@Override
	public void process(final Reader reader, final TokenBuilder tokenBuilder) {
		final boolean isEofErrorP = tokenBuilder.isEofErrorP();
		final LispStruct eofValue = tokenBuilder.getEofValue();
		final boolean isRecursiveP = tokenBuilder.isRecursiveP();

		final ReadResult readResult = reader.readChar(isEofErrorP, eofValue, isRecursiveP);
		if (readResult.wasEOF()) {
			IllegalCharacterState.INSTANCE.process(reader, tokenBuilder);
		} else {
			final int codePoint = readResult.getResult();
			tokenBuilder.setPreviousReadCharacter(codePoint);

			tokenBuilder.addToTokenAttributes(codePoint, AttributeType.ALPHABETIC);

			EvenMultiEscapeState.INSTANCE.process(reader, tokenBuilder);
		}
	}
}
