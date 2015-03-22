/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams;

import jcl.LispStruct;
import jcl.conditions.exceptions.StreamErrorException;
import jcl.types.Character;
import jcl.types.Stream;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.nio.charset.Charset;

/**
 * The {@link CharacterStreamStruct} is the object representation of a character reading system level Lisp stream.
 */
public class CharacterStreamStruct extends AbstractNativeStreamStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 3029213066284401689L;

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(CharacterStreamStruct.class);

	/**
	 * The {@link LineNumberReader} for reading input.
	 */
	private final LineNumberReader inputStream;

	/**
	 * The {@link PrintWriter} for writing output.
	 */
	private final PrintWriter outputStream;

	/**
	 * Public constructor.
	 *
	 * @param inputStream
	 * 		the {@link java.io.InputStream} to create a CharacterStreamStruct from
	 * @param outputStream
	 * 		the {@link java.io.OutputStream} to create a CharacterStreamStruct from
	 */
	public CharacterStreamStruct(final InputStream inputStream, final OutputStream outputStream) {
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
	public CharacterStreamStruct(final boolean interactive, final InputStream inputStream, final OutputStream outputStream) {
		super(Stream.INSTANCE, interactive, Character.INSTANCE);

		final Charset defaultCharset = Charset.defaultCharset();
		this.inputStream = new LineNumberReader(new InputStreamReader(inputStream, defaultCharset));
		this.outputStream = new PrintWriter(new OutputStreamWriter(outputStream, defaultCharset));
	}

	@Override
	public ReadPeekResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		try {
			inputStream.mark(1);
			final int readChar = inputStream.read();
			return StreamUtils.getReadPeekResult(readChar, eofErrorP, eofValue);
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_READ_CHAR, ioe);
		}
	}

	@Override
	public ReadPeekResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		throw new StreamErrorException(StreamUtils.OPERATION_ONLY_BINARY_STREAM);
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

		return StreamUtils.getReadPeekResult(nextChar, eofErrorP, eofValue);
	}

	/**
	 * Attempts to peek ahead to the next available character in the stream.
	 *
	 * @return the character peeked from the stream
	 */
	private int nilPeekCharCSS() {
		try {
			inputStream.mark(1);

			final int nextChar = inputStream.read();
			inputStream.reset();
			return nextChar;
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_PEEK_CHAR, ioe);
		}
	}

	/**
	 * Attempts to peek ahead to the next available non-whitespace character in the stream.
	 *
	 * @return the character peeked from the stream
	 */
	private int tPeekCharCSS() {
		try {
			inputStream.mark(1);

			// Initialize to whitespace, since we are attempting to skip it anyways
			int nextChar = ' ';
			while (java.lang.Character.isWhitespace(nextChar)) {
				nextChar = inputStream.read();
			}
			inputStream.reset();
			return nextChar;
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_PEEK_CHAR, ioe);
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
			inputStream.mark(1);

			// Initialize to -1 value, since this is essentially EOF
			int nextChar = -1;
			while (nextChar != codePoint) {
				nextChar = inputStream.read();
			}
			inputStream.reset();
			return nextChar;
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_PEEK_CHAR, ioe);
		}
	}

	@Override
	public Integer unreadChar(final Integer codePoint) {
		try {
			inputStream.reset();
			return codePoint;
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_UNREAD_CHAR, ioe);
		}
	}

	@Override
	public void clearInput() {
		try {
			inputStream.mark(0);
			inputStream.reset();
		} catch (final IOException ioe) {
			if (LOGGER.isWarnEnabled()) {
				LOGGER.warn("IO exception occurred.", ioe);
			}
		}
	}

	@Override
	public void writeChar(final int aChar) {
		outputStream.append((char) aChar);
	}

	@Override
	public void writeByte(final int aByte) {
		throw new StreamErrorException(StreamUtils.OPERATION_ONLY_BINARY_STREAM);
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
	public void close() {
		try {
			inputStream.close();
			outputStream.close();
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not close stream.", ioe);
		}
		super.close();
	}

	@Override
	public Long fileLength() {
		throw new StreamErrorException(StreamUtils.OPERATION_ONLY_FILE_STREAM);
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
		final CharacterStreamStruct rhs = (CharacterStreamStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(inputStream, rhs.inputStream)
		                          .append(outputStream, rhs.outputStream)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(inputStream)
		                                                                .append(outputStream)
		                                                                .toString();
	}
}
