/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.stream;

import jcl.lang.LispStruct;

/**
 * The {@link InputStream} is the representation for all Lisp input 'stream' types.
 */
public interface InputStream extends LispStream {

	/**
	 * Reads a character from the stream.
	 *
	 * @param eofErrorP
	 * 		whether or not to either throw an error or return gracefully
	 * @param eofValue
	 * 		the graceful return value when an error occurs
	 * @param recursiveP
	 * 		whether or not to recursively read and process the character
	 *
	 * @return the character read from the stream
	 */
	ReadPeekResult readChar(boolean eofErrorP, LispStruct eofValue, boolean recursiveP);

	/**
	 * Reads a byte from the stream.
	 *
	 * @param eofErrorP
	 * 		whether or not to either throw an error or return gracefully
	 * @param eofValue
	 * 		the graceful return value when an error occurs
	 *
	 * @return the byte read from the stream
	 */
	ReadPeekResult readByte(boolean eofErrorP, LispStruct eofValue);

	/**
	 * Peeks at the next available character in the stream.
	 *
	 * @param peekType
	 * 		how to peek and find the next character in the stream
	 * @param eofErrorP
	 * 		whether or not to either throw an error or return gracefully
	 * @param eofValue
	 * 		the graceful return value when an error occurs
	 * @param recursiveP
	 * 		whether or not to recursively read and process the character
	 *
	 * @return the next character available in the stream
	 */
	ReadPeekResult peekChar(PeekType peekType, boolean eofErrorP, LispStruct eofValue, boolean recursiveP);

	/**
	 * Un-reads a character from the stream.
	 *
	 * @param codePoint
	 * 		the codePoint value to un-read back into the stream
	 *
	 * @return the codePoint un-read back into the stream
	 */
	Integer unreadChar(Integer codePoint);

	/**
	 * Clears the input from the stream.
	 */
	void clearInput();

	/**
	 * Listens on the stream to determine if there is any data left to read.
	 *
	 * @return whether or not there is data left to read from the stream
	 */
	boolean listen();

	@Override
	default boolean isInputStream() {
		return true;
	}

	default ReadLineResult readLine(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {

		final StringBuilder stringBuilder = new StringBuilder();

		ReadPeekResult readPeekResult = readChar(eofErrorP, eofValue, recursiveP);
		Integer result = readPeekResult.getResult();
		while (!readPeekResult.isEof() && (result != '\n')) {
			stringBuilder.appendCodePoint(result);

			readPeekResult = readChar(eofErrorP, eofValue, recursiveP);
			result = readPeekResult.getResult();
		}
		final String resultString = stringBuilder.toString();
		return new ReadLineResult(resultString, readPeekResult.isEof());
	}
}
