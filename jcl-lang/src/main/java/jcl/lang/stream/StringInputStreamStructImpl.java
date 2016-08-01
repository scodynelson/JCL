/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.stream;

import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.StreamErrorException;
import jcl.type.BaseCharType;
import jcl.type.StringStreamType;

/**
 * The {@link StringInputStreamStructImpl} is the object representation of a Lisp 'string-stream' input type.
 */
public final class StringInputStreamStructImpl extends StreamStructImpl implements InputStreamStruct {

	/**
	 * The {@link String} input value to read characters from.
	 */
	private final String inputString;

	/**
	 * The length of the {@link #inputString}.
	 */
	private final int end;

	/**
	 * The current location of reads on the {@link #inputString}.
	 */
	private int current;

	/**
	 * Public constructor.
	 *
	 * @param inputString
	 * 		the input to create a StringInputStreamStruct from
	 */
	private StringInputStreamStructImpl(final String inputString) {
		this(false, inputString);
	}

	/**
	 * Public constructor.
	 *
	 * @param interactive
	 * 		whether or not the struct created is 'interactive'
	 * @param inputString
	 * 		the input to create a StringInputStreamStruct from
	 */
	private StringInputStreamStructImpl(final boolean interactive, final String inputString) {
		this(interactive, inputString, 0, inputString.length());
	}

	/**
	 * Public constructor.
	 *
	 * @param inputString
	 * 		the input to create a StringInputStreamStruct from
	 * @param current
	 * 		the current position to read from in the string
	 * @param end
	 * 		the ending position to read up to in the string
	 */
	private StringInputStreamStructImpl(final String inputString, final int current, final int end) {
		this(false, inputString, current, end);
	}

	/**
	 * Public constructor.
	 *
	 * @param interactive
	 * 		whether or not the struct created is 'interactive'
	 * @param inputString
	 * 		the input to create a StringInputStreamStruct from
	 * @param current
	 * 		the current position to read from in the string
	 * @param end
	 * 		the ending position to read up to in the string
	 */
	private StringInputStreamStructImpl(final boolean interactive, final String inputString, final int current, final int end) {
		super(StringStreamType.INSTANCE, null, null, interactive, BaseCharType.INSTANCE);

		if (inputString == null) {
			throw new ErrorException("Provided Input String must not be null.");
		}
		this.inputString = inputString;

		this.end = end;
		this.current = current;
	}

	public static StringInputStreamStructImpl valueOf(final String inputString) {
		return new StringInputStreamStructImpl(inputString);
	}

	public static StringInputStreamStructImpl valueOf(final boolean interactive, final String inputString) {
		return new StringInputStreamStructImpl(interactive, inputString);
	}

	public static StringInputStreamStructImpl valueOf(final String inputString, final int current, final int end) {
		return new StringInputStreamStructImpl(inputString, current, end);
	}

	public static StringInputStreamStructImpl valueOf(final boolean interactive, final String inputString, final int current, final int end) {
		return new StringInputStreamStructImpl(interactive, inputString, current, end);
	}

	@Override
	public ReadPeekResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		if (current == end) {
			if (eofErrorP) {
				return new ReadPeekResult(10);
			} else {
				return new ReadPeekResult(eofValue);
			}
		}

		final int readChar = inputString.charAt(current);
		if (readChar == '\n') {
			lineNumber++;
		}
		current++;
		return new ReadPeekResult(readChar);
	}

	@Override
	public ReadPeekResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		throw new StreamErrorException(StreamUtils.OPERATION_ONLY_BINARY_STREAM, this);
	}

	@Override
	public ReadPeekResult peekChar(final PeekType peekType, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		if ((current + 1) == end) {
			if (eofErrorP) {
				return new ReadPeekResult(10);
			} else {
				return new ReadPeekResult(eofValue);
			}
		}

		final int nextChar;
		switch (peekType.getType()) {
			case NIL:
				nextChar = nilPeekCharSIS();
				break;
			case T:
				nextChar = tPeekCharSIS();
				break;
			case CHARACTER:
				nextChar = characterPeekCharSIS(peekType.getCodePoint());
				break;
			default:
				nextChar = -1;
				break;
		}

		return StreamUtils.getReadPeekResult(this, nextChar, eofErrorP, eofValue);
	}

	/**
	 * Attempts to peek ahead to the next available character in the stream.
	 *
	 * @return the character peeked from the stream
	 */
	private int nilPeekCharSIS() {
		return inputString.charAt(current + 1);
	}

	/**
	 * Attempts to peek ahead to the next available non-whitespace character in the stream.
	 *
	 * @return the character peeked from the stream
	 */
	private int tPeekCharSIS() {
		// Initialize to whitespace, since we are attempting to skip it anyways
		int nextChar = ' ';

		int indexToFind = current + 1;
		while (Character.isWhitespace(nextChar) && (indexToFind < end)) {
			nextChar = inputString.charAt(current + indexToFind);
			indexToFind++;
		}

		return (indexToFind == end) ? nextChar : -1;
	}

	/**
	 * Attempts to peek ahead to the provided {@code codePoint} in the stream.
	 *
	 * @param codePoint
	 * 		the codePoint to peek up to in the stream
	 *
	 * @return the character peeked from the stream
	 */
	private int characterPeekCharSIS(final Integer codePoint) {
		// Initialize to -1 value, since this is essentially EOF
		int nextChar = -1;

		int indexToFind = current + 1;
		while ((nextChar != codePoint) && (indexToFind < end)) {
			nextChar = inputString.charAt(current + indexToFind);
			indexToFind++;
		}

		return (indexToFind == end) ? nextChar : -1;
	}

	@Override
	public Integer unreadChar(final Integer codePoint) {
		if (codePoint == '\n') {
			lineNumber--;
		}
		current--;
		return codePoint;
	}

	@Override
	public void clearInput() {
		// Do nothing.
	}

	@Override
	public boolean listen() {
		return current < end;
	}

	@Override
	public Long fileLength() {
		throw new StreamErrorException(StreamUtils.OPERATION_ONLY_FILE_STREAM, this);
	}

	@Override
	public Long filePosition(final Long filePosition) {
		if (filePosition != null) {
			current = filePosition.intValue();
		}
		return (long) current;
	}
}