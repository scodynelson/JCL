/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams;

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

import jcl.LispStruct;
import jcl.conditions.exceptions.StreamErrorException;
import jcl.types.CharacterType;
import jcl.types.StreamType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * The {@link JavaStreamStruct} is the object representation of a character reading and writing system level Lisp
 * stream.
 */
public class JavaStreamStruct extends AbstractNativeStreamStruct {

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
	 * Public constructor.
	 *
	 * @param inputStream
	 * 		the {@link InputStream} to create a CharacterStreamStruct from
	 * @param outputStream
	 * 		the {@link OutputStream} to create a CharacterStreamStruct from
	 */
	public JavaStreamStruct(final InputStream inputStream, final OutputStream outputStream) {
		this(false, inputStream, outputStream);
	}

	/**
	 * Public constructor.
	 *
	 * @param interactive
	 * 		whether or not the struct created is 'interactive'
	 * @param inputStream
	 * 		the {@link InputStream} to create a CharacterStreamStruct from
	 * @param outputStream
	 * 		the {@link OutputStream} to create a CharacterStreamStruct from
	 */
	public JavaStreamStruct(final boolean interactive, final InputStream inputStream, final OutputStream outputStream) {
		super(StreamType.INSTANCE, interactive, CharacterType.INSTANCE);

		final Charset defaultCharset = Charset.defaultCharset();
		this.inputStream = new PushbackReader(new InputStreamReader(inputStream, defaultCharset), PUSHBACK_BUFFER_SIZE);
		this.outputStream = new PrintWriter(new OutputStreamWriter(outputStream, defaultCharset));
	}

	@Override
	public ReadPeekResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		try {
			final int readChar = inputStream.read();
			return StreamUtils.getReadPeekResult(this, readChar, eofErrorP, eofValue);
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_READ_CHAR, ioe, this);
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
			inputStream.unread(codePoint);
			return codePoint;
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_UNREAD_CHAR, ioe, this);
		}
	}

	@Override
	public void clearInput() {
		// Do nothing.
	}

	@Override
	public void writeChar(final int aChar) {
		outputStream.append((char) aChar);
	}

	@Override
	public void writeByte(final int aByte) {
		throw new StreamErrorException(StreamUtils.OPERATION_ONLY_BINARY_STREAM, this);
	}

	@Override
	public void writeString(final String outputString, final int start, final int end) {
		outputStream.append(outputString, start, end);
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
		return false;
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

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(inputStream)
		                            .append(outputStream)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final JavaStreamStruct rhs = (JavaStreamStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(inputStream, rhs.inputStream)
		                          .append(outputStream, rhs.outputStream)
		                          .isEquals();
	}
}