/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.state;

import jcl.characters.CharacterConstants;
import jcl.compiler.real.element.NullElement;
import jcl.compiler.real.element.SimpleElement;
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
	public SimpleElement process(final TokenBuilder tokenBuilder) {

		if (!tokenBuilder.isEofErrorP()) {
//			return tokenBuilder.getEofValue();
			return NullElement.INSTANCE; // TODO: handle Eof values
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

		return null;
	}
}
