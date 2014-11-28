/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.state;

import jcl.conditions.exceptions.ReaderErrorException;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;

import java.util.List;

/**
 * Step 10 of the Reader Algorithm.
 * <p>
 * An entire token has been accumulated. The object represented by the token is returned as the result of the read
 * operation, or an error of type reader-error is signaled if the token is not of valid syntax.
 * </p>
 * <p>
 * This state is reached when we have accumulated a token, and it needs to be processed into either
 * 1) Number/PotentialNumber
 * 2) Symbol
 * 3) Package with a Symbol
 * </p>
 */
final class TokenAccumulatedState implements State {

	/**
	 * Singleton instance variable.
	 */
	static final State INSTANCE = new TokenAccumulatedState();

	/**
	 * Private constructor.
	 */
	private TokenAccumulatedState() {
	}

	@Override
	public void process(final ReaderStateMediator readerStateMediator, final Reader reader, final TokenBuilder tokenBuilder) {
		final Integer codePoint = tokenBuilder.getPreviousReadCharacter();

		final List<TokenAttribute> tokenAttributes = tokenBuilder.getTokenAttributes();
		if (State.isEndOfFileCharacter(codePoint) && tokenAttributes.isEmpty()) {
			State.handleEndOfFile(tokenBuilder, "TokenAccumulatedState");
			return;
		}

		if (ReaderVariables.READ_SUPPRESS.getValue().booleanValue()) {
			tokenBuilder.setReturnToken(null);
			return;
		}

		final String tokenString = State.convertTokensToString(tokenAttributes);
		if (".".equals(tokenString)) {
			throw new ReaderErrorException("Dot context error in '.'");
		} else {
			NumberTokenAccumulatedState.INSTANCE.process(readerStateMediator, reader, tokenBuilder);
		}
	}
}
