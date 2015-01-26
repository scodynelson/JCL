/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams;

import jcl.LispType;
import jcl.conditions.exceptions.StreamErrorException;
import jcl.types.Stream;
import jcl.types.typespecifiers.AndTypeSpecifier;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link AbstractDualStreamStruct} is an abstraction for dual stream types.
 */
abstract class AbstractDualStreamStruct extends StreamStruct implements InputStream, OutputStream {

	private static final long serialVersionUID = -3285885552701366356L;

	/**
	 * This {@link InputStream} in the DualStreamStruct.
	 */
	protected final InputStream inputStream;

	/**
	 * This {@link OutputStream} in the DualStreamStruct.
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
	 * 		the {@link InputStream} to create a DualStreamStruct from
	 * @param outputStream
	 * 		the {@link OutputStream} to create a DualStreamStruct from
	 */
	protected AbstractDualStreamStruct(final Stream type,
	                                   final boolean interactive, final InputStream inputStream, final OutputStream outputStream) {
		super(type, null, null, interactive, getElementType(inputStream, outputStream));
		this.inputStream = inputStream;
		this.outputStream = outputStream;
	}

	/**
	 * This method is used to retrieve the element type for object construction.
	 *
	 * @param inputStream
	 * 		the {@link InputStream} to create a DualStreamStruct from
	 * @param outputStream
	 * 		the {@link OutputStream} to create a DualStreamStruct from
	 *
	 * @return the element type for object construction
	 */
	private static LispType getElementType(final InputStream inputStream, final OutputStream outputStream) {
		if (inputStream == null) {
			throw new StreamErrorException("Provided Input Stream must not be null.");
		}
		if (outputStream == null) {
			throw new StreamErrorException("Provided Output Stream must not be null.");
		}

		final LispType inType = inputStream.getElementType();
		final LispType outType = outputStream.getElementType();

		return inType.equals(outType) ? inType : new AndTypeSpecifier(inType, outType);
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
		throw new StreamErrorException(StreamUtils.OPERATION_ONLY_FILESTREAM);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
