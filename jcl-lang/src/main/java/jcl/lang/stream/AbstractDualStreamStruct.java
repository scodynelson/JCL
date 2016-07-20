/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.stream;

import jcl.type.LispType;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.StreamErrorException;
import jcl.type.StreamType;
import jcl.type.typespecifier.AndTypeSpecifier;

/**
 * The {@link AbstractDualStreamStruct} is an abstraction for dual stream types.
 */
abstract class AbstractDualStreamStruct extends StreamStruct implements IOStream {

	/**
	 * This {@link InputStream} in the AbstractDualStreamStruct.
	 */
	protected final InputStream inputStream;

	/**
	 * This {@link OutputStream} in the AbstractDualStreamStruct.
	 */
	protected final OutputStream outputStream;

	/**
	 * Protected constructor.
	 *
	 * @param type
	 * 		the type of the stream object
	 * @param interactive
	 * 		whether or not the struct created is 'interactive'
	 * @param inputStream
	 * 		the {@link InputStream} to create an AbstractDualStreamStruct from
	 * @param outputStream
	 * 		the {@link OutputStream} to create an AbstractDualStreamStruct from
	 */
	protected AbstractDualStreamStruct(final StreamType type,
	                                   final boolean interactive, final InputStream inputStream, final OutputStream outputStream) {
		super(type, null, null, interactive, getElementType(inputStream, outputStream));
		this.inputStream = inputStream;
		this.outputStream = outputStream;
	}

	/**
	 * This method is used to retrieve the element type for object construction.
	 *
	 * @param inputStream
	 * 		the {@link InputStream} to create an AbstractDualStreamStruct from
	 * @param outputStream
	 * 		the {@link OutputStream} to create an AbstractDualStreamStruct from
	 *
	 * @return the element type for object construction
	 */
	private static LispType getElementType(final InputStream inputStream, final OutputStream outputStream) {
		if (inputStream == null) {
			throw new ErrorException("Provided Input Stream must not be null.");
		}
		if (outputStream == null) {
			throw new ErrorException("Provided Output Stream must not be null.");
		}

		final LispType inType = inputStream.getElementType();
		final LispType outType = outputStream.getElementType();

		return inType.equals(outType) ? inType : new AndTypeSpecifier(inType, outType);
	}

	/**
	 * Getter for {@link #inputStream} property.
	 *
	 * @return {@link #inputStream} property
	 */
	public InputStream getInputStream() {
		return inputStream;
	}

	/**
	 * Getter for {@link #outputStream} property.
	 *
	 * @return {@link #outputStream} property
	 */
	public OutputStream getOutputStream() {
		return outputStream;
	}

	@Override
	public void writeChar(final int aChar) {
		outputStream.writeChar(aChar);
	}

	@Override
	public void writeByte(final int aByte) {
		outputStream.writeByte(aByte);
	}

	@Override
	public void writeString(final String outputString, final int start, final int end) {
		outputStream.writeString(outputString, start, end);
	}

	@Override
	public void clearOutput() {
		outputStream.clearOutput();
	}

	@Override
	public void finishOutput() {
		outputStream.finishOutput();
	}

	@Override
	public void forceOutput() {
		outputStream.forceOutput();
	}

	@Override
	public void clearInput() {
		inputStream.clearInput();
	}

	@Override
	public boolean listen() {
		return inputStream.listen();
	}

	@Override
	public Long fileLength() {
		throw new StreamErrorException(StreamUtils.OPERATION_ONLY_FILE_STREAM, this);
	}

	@Override
	public boolean isStartOfLine() {
		return outputStream.isStartOfLine();
	}
}