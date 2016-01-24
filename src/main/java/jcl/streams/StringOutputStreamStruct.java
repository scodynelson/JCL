/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams;

import jcl.LispType;
import jcl.conditions.exceptions.StreamErrorException;
import jcl.types.BaseCharType;
import jcl.types.StringStreamType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * The {@link StringOutputStreamStruct} is the object representation of a Lisp 'string-stream' output type.
 */
public class StringOutputStreamStruct extends StreamStruct implements OutputStream {

	/**
	 * The {@link StringBuffer} to use for this stream to accept characters and bytes.
	 */
	private final StringBuilder stringBuffer = new StringBuilder();

	/**
	 * Public constructor.
	 */
	public StringOutputStreamStruct() {
		this(false);
	}

	/**
	 * Public constructor.
	 *
	 * @param interactive
	 * 		whether or not the struct created is 'interactive'
	 */
	public StringOutputStreamStruct(final boolean interactive) {
		this(interactive, BaseCharType.INSTANCE);
	}

	/**
	 * Public constructor.
	 *
	 * @param elementType
	 * 		the type of character elements in the stream
	 */
	public StringOutputStreamStruct(final LispType elementType) {
		this(false, elementType);
	}

	/**
	 * Public constructor.
	 *
	 * @param interactive
	 * 		whether or not the struct created is 'interactive'
	 * @param elementType
	 * 		the type of character elements in the stream
	 */
	public StringOutputStreamStruct(final boolean interactive, final LispType elementType) {
		super(StringStreamType.INSTANCE, null, null, interactive, elementType);
	}

	public String getStreamString() {
		return stringBuffer.toString();
	}

	public void clearStream() {
		stringBuffer.setLength(0);
	}

	@Override
	public void writeChar(final int aChar) {
		stringBuffer.appendCodePoint(aChar);
	}

	@Override
	public void writeByte(final int aByte) {
		throw new StreamErrorException(StreamUtils.OPERATION_ONLY_BINARY_STREAM);
	}

	@Override
	public void writeString(final String outputString, final int start, final int end) {
		final String subString = outputString.substring(start, end);
		stringBuffer.append(subString);
	}

	@Override
	public void clearOutput() {
		// Do nothing.
	}

	@Override
	public void finishOutput() {
		// Do nothing.
	}

	@Override
	public void forceOutput() {
		// Do nothing.
	}

	@Override
	public Long fileLength() {
		throw new StreamErrorException(StreamUtils.OPERATION_ONLY_FILE_STREAM);
	}

	@Override
	public Long filePosition(final Long filePosition) {
		if (filePosition == null) {
			return (long) stringBuffer.length();
		}
		return null;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(stringBuffer)
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
		final StringOutputStreamStruct rhs = (StringOutputStreamStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(stringBuffer, rhs.stringBuffer)
		                          .isEquals();
	}
}
