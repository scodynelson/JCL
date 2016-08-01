/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.stream;

import jcl.lang.condition.exception.StreamErrorException;
import jcl.type.BaseCharType;
import jcl.type.LispType;
import jcl.type.StringStreamType;

/**
 * The {@link StringOutputStreamStructImpl} is the object representation of a Lisp 'string-stream' output type.
 */
public final class StringOutputStreamStructImpl extends StreamStructImpl implements OutputStreamStruct {

	/**
	 * The {@link StringBuffer} to use for this stream to accept characters and bytes.
	 */
	private final StringBuilder stringBuffer = new StringBuilder();

	/**
	 * Public constructor.
	 */
	private StringOutputStreamStructImpl() {
		this(false);
	}

	/**
	 * Public constructor.
	 *
	 * @param interactive
	 * 		whether or not the struct created is 'interactive'
	 */
	private StringOutputStreamStructImpl(final boolean interactive) {
		this(interactive, BaseCharType.INSTANCE);
	}

	/**
	 * Public constructor.
	 *
	 * @param elementType
	 * 		the type of character elements in the stream
	 */
	private StringOutputStreamStructImpl(final LispType elementType) {
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
	private StringOutputStreamStructImpl(final boolean interactive, final LispType elementType) {
		super(StringStreamType.INSTANCE, null, null, interactive, elementType);
	}

	public static StringOutputStreamStructImpl valueOf() {
		return new StringOutputStreamStructImpl();
	}

	public static StringOutputStreamStructImpl valueOf(final boolean interactive) {
		return new StringOutputStreamStructImpl(interactive);
	}

	public static StringOutputStreamStructImpl valueOf(final LispType elementType) {
		return new StringOutputStreamStructImpl(elementType);
	}

	public static StringOutputStreamStructImpl valueOf(final boolean interactive, final LispType elementType) {
		return new StringOutputStreamStructImpl(interactive, elementType);
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
		throw new StreamErrorException(StreamUtils.OPERATION_ONLY_BINARY_STREAM, this);
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
	public boolean isStartOfLine() {
		return stringBuffer.charAt(stringBuffer.length() - 1) == '\n';
	}

	@Override
	public Long fileLength() {
		throw new StreamErrorException(StreamUtils.OPERATION_ONLY_FILE_STREAM, this);
	}

	@Override
	public Long filePosition(final Long filePosition) {
		if (filePosition == null) {
			return (long) stringBuffer.length();
		}
		return null;
	}
}
