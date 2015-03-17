/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams;

import jcl.conditions.exceptions.StreamErrorException;
import jcl.types.BaseChar;
import jcl.types.StringStream;

/**
 * The {@link StringOutputStreamStruct} is the object representation of a Lisp 'string-stream' output type.
 */
public class StringOutputStreamStruct extends StreamStruct implements OutputStream {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -6102057416932242456L;

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
		super(StringStream.INSTANCE, null, null, interactive, BaseChar.INSTANCE);
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
}
