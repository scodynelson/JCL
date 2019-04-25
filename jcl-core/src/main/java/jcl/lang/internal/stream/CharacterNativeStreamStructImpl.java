/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal.stream;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.PushbackReader;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.List;

import jcl.lang.CharacterNativeStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.condition.exception.StreamErrorException;
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.stream.PeekType;
import jcl.lang.stream.ReadPeekResult;
import org.apache.commons.lang3.SystemUtils;

/**
 * The {@link CharacterNativeStreamStructImpl} is the object representation of a character reading and writing system level Lisp
 * stream.
 */
public final class CharacterNativeStreamStructImpl extends AbstractNativeStreamStructImpl implements CharacterNativeStreamStruct {

	/**
	 * The maximum size of internal buffer array to allocate in the {@link PushbackReader} {@link #inputStream}.
	 */
	private static final int PUSHBACK_BUFFER_SIZE = Short.MAX_VALUE;

	/**
	 * The {@link PushbackReader} for reading input.
	 */
	private final PushbackReader inputStream;

	/**
	 * The {@link PrintWriter} for writing output.
	 */
	private final PrintWriter outputStream;

	/**
	 * The last character read from the {@link #inputStream}.
	 */
	private char lastChar;

	/**
	 * Public constructor.
	 *
	 * @param inputStream
	 * 		the {@link java.io.InputStream} to create a CharacterStreamStruct from
	 * @param outputStream
	 * 		the {@link java.io.OutputStream} to create a CharacterStreamStruct from
	 */
	public CharacterNativeStreamStructImpl(final InputStream inputStream, final OutputStream outputStream) {
		this(false, inputStream, outputStream);
	}

	/**
	 * Public constructor.
	 *
	 * @param interactive
	 * 		whether or not the struct created is 'interactive'
	 * @param inputStream
	 * 		the {@link java.io.InputStream} to create a CharacterStreamStruct from
	 * @param outputStream
	 * 		the {@link java.io.OutputStream} to create a CharacterStreamStruct from
	 */
	public CharacterNativeStreamStructImpl(final boolean interactive, final InputStream inputStream, final OutputStream outputStream) {
		super(interactive, CommonLispSymbols.CHARACTER);

		final Charset defaultCharset = Charset.defaultCharset();
		this.inputStream = new PushbackReader(new InputStreamReader(inputStream, defaultCharset), PUSHBACK_BUFFER_SIZE);
		this.outputStream = new PrintWriter(new OutputStreamWriter(outputStream, defaultCharset));
	}

	@Override
	public ReadPeekResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		try {
			final int readChar = inputStream.read();
			final ReadPeekResult readPeekResult = StreamUtils.getReadPeekResult(this, readChar, eofErrorP, eofValue);
			if ((readChar == '\r') && SystemUtils.IS_OS_WINDOWS) {
				return handleWindowsEOLChar(eofErrorP, eofValue, recursiveP, readPeekResult);
			}
			if (readChar == '\n') {
				lineNumber++;
			}
			return readPeekResult;
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_READ_CHAR, ioe, this);
		}
	}

	private ReadPeekResult handleWindowsEOLChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP,
	                                            final ReadPeekResult readPeekResult) {
		final ReadPeekResult tempReadPeekResult = readChar(eofErrorP, eofValue, recursiveP);
		final Integer result = tempReadPeekResult.getResult();
		if (result == null) {
			return tempReadPeekResult;
		}

		if (result == '\n') {
			lineNumber++;
			return tempReadPeekResult;
		} else {
			unreadChar(result);
			return readPeekResult;
		}
	}

	@Override
	public ReadPeekResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		throw new StreamErrorException(StreamUtils.OPERATION_ONLY_BINARY_STREAM, this);
	}

	@Override
	public ReadPeekResult peekChar(final PeekType peekType, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {

		final int nextChar;
		switch (peekType.getType()) {
			case NIL:
				nextChar = nilPeekCharCSS();
				break;
			case T:
				nextChar = tPeekCharCSS();
				break;
			case CHARACTER:
				nextChar = characterPeekCharCSS(peekType.getCodePoint());
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
	private int nilPeekCharCSS() {
		try {
			final int nextChar = inputStream.read();
			inputStream.unread(nextChar);
			return nextChar;
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_PEEK_CHAR, ioe, this);
		}
	}

	/**
	 * Attempts to peek ahead to the next available non-whitespace character in the stream.
	 *
	 * @return the character peeked from the stream
	 */
	private int tPeekCharCSS() {
		try {
			final List<Integer> charsToUnread = new ArrayList<>();

			// Initialize to whitespace, since we are attempting to skip it anyways
			int nextChar = ' ';
			while (Character.isWhitespace(nextChar)) {
				nextChar = inputStream.read();
				charsToUnread.add(nextChar);
			}
			for (final Integer charToUnread : charsToUnread) {
				// This will insert back in the correct order.
				inputStream.unread(charToUnread);
			}
			return nextChar;
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_PEEK_CHAR, ioe, this);
		}
	}

	/**
	 * Attempts to peek ahead to the provided {@code codePoint} in the stream.
	 *
	 * @param codePoint
	 * 		the codePoint to peek up to in the stream
	 *
	 * @return the character peeked from the stream
	 */
	private int characterPeekCharCSS(final Integer codePoint) {
		try {
			final List<Integer> charsToUnread = new ArrayList<>();

			// Initialize to -1 value, since this is essentially EOF
			int nextChar = -1;
			while (nextChar != codePoint) {
				nextChar = inputStream.read();
				charsToUnread.add(nextChar);
			}
			for (final Integer charToUnread : charsToUnread) {
				// This will insert back in the correct order.
				inputStream.unread(charToUnread);
			}
			return nextChar;
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_PEEK_CHAR, ioe, this);
		}
	}

	@Override
	public Integer unreadChar(final Integer codePoint) {
		try {
			if (codePoint == '\n') {
				lineNumber--;
			}
			inputStream.unread(codePoint);
			return codePoint;
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_UNREAD_CHAR, ioe, this);
		}
	}

	@Override
	public void clearInput() {
		try {
			ReadPeekResult result = new ReadPeekResult(0);
			while (inputStream.ready() && !result.isEof()) {
				result = readChar(false, null, false);
			}
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not clear input for stream.", ioe, this);
		}
	}

	@Override
	public void writeChar(final int aChar) {
		if ((aChar == '\n') && SystemUtils.IS_OS_WINDOWS && (lastChar != '\r')) {
			outputStream.write('\r');
			outputStream.write('\n');
			lastChar = '\n';
			outputStream.flush();
		} else {
			outputStream.write((char) aChar);
			lastChar = (char) aChar;
		}
	}

	@Override
	public void writeByte(final int aByte) {
		throw new StreamErrorException(StreamUtils.OPERATION_ONLY_BINARY_STREAM, this);
	}

	@Override
	public void writeString(final String outputString, final int start, final int end) {
		final String substring = outputString.substring(start, end);
		outputStream.write(substring);

		lastChar = outputString.charAt(end - 1);
	}

	@Override
	public void clearOutput() {
		outputStream.flush();
	}

	@Override
	public void finishOutput() {
		outputStream.flush();
	}

	@Override
	public void forceOutput() {
		outputStream.flush();
	}

	@Override
	public boolean isStartOfLine() {
		return lastChar == '\n';
	}

	@Override
	public boolean close() {
		try {
			inputStream.close();
			outputStream.close();
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not close stream.", ioe, this);
		}
		return super.close();
	}

	@Override
	public Long fileLength() {
		throw new StreamErrorException(StreamUtils.OPERATION_ONLY_FILE_STREAM, this);
	}

	@Override
	public Long filePosition(final Long filePosition) {
		return null;
	}
}
