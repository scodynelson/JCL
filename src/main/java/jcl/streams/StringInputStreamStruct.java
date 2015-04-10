/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams;

import jcl.LispStruct;
import jcl.conditions.exceptions.EndOfFileException;
import jcl.conditions.exceptions.StreamErrorException;
import jcl.types.BaseCharType;
import jcl.types.StringStreamType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link StringInputStreamStruct} is the object representation of a Lisp 'string-stream' input type.
 */
public class StringInputStreamStruct extends StreamStruct implements InputStream {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 4439375845861585598L;

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
	public StringInputStreamStruct(final String inputString) {
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
	public StringInputStreamStruct(final boolean interactive, final String inputString) {
		super(StringStreamType.INSTANCE, null, null, interactive, BaseCharType.INSTANCE);

		if (inputString == null) {
			throw new StreamErrorException("Provided Input String must not be null.");
		}
		this.inputString = inputString;

		end = inputString.length();
		current = 0;
	}

	@Override
	public ReadPeekResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		if (current == end) {
			if (eofErrorP) {
				throw new EndOfFileException(StreamUtils.END_OF_FILE_REACHED);
			} else {
				return new ReadPeekResult(eofValue);
			}
		}

		current++;
		final int readChar = inputString.charAt(current);
		return new ReadPeekResult(readChar);
	}

	@Override
	public ReadPeekResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		throw new StreamErrorException(StreamUtils.OPERATION_ONLY_BINARY_STREAM);
	}

	@Override
	public ReadPeekResult peekChar(final PeekType peekType, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		if ((current + 1) == end) {
			if (eofErrorP) {
				throw new EndOfFileException(StreamUtils.END_OF_FILE_REACHED);
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

		return StreamUtils.getReadPeekResult(nextChar, eofErrorP, eofValue);
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
		throw new StreamErrorException(StreamUtils.OPERATION_ONLY_FILE_STREAM);
	}

	@Override
	public Long filePosition(final Long filePosition) {
		if (filePosition != null) {
			current = filePosition.intValue();
		}
		return (long) current;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(inputString)
		                            .append(end)
		                            .append(current)
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
		final StringInputStreamStruct rhs = (StringInputStreamStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(inputString, rhs.inputString)
		                          .append(end, rhs.end)
		                          .append(current, rhs.current)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(inputString)
		                                                                .append(end)
		                                                                .append(current)
		                                                                .toString();
	}
}
