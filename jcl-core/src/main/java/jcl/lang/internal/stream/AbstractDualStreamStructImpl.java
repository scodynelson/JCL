/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal.stream;

import jcl.lang.DualStreamStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.OutputStreamStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.StreamErrorException;
import jcl.lang.statics.CommonLispSymbols;

/**
 * The {@link AbstractDualStreamStructImpl} is an abstraction for dual stream types.
 */
abstract class AbstractDualStreamStructImpl extends StreamStructImpl implements DualStreamStruct {

	/**
	 * This {@link InputStreamStruct} in the AbstractDualStreamStruct.
	 */
	protected final InputStreamStruct inputStreamStruct;

	/**
	 * This {@link OutputStreamStruct} in the AbstractDualStreamStruct.
	 */
	protected final OutputStreamStruct outputStreamStruct;

	/**
	 * Protected constructor.
	 *
	 * @param interactive
	 * 		whether or not the struct created is 'interactive'
	 * @param inputStreamStruct
	 * 		the {@link InputStreamStruct} to create an AbstractDualStreamStruct from
	 * @param outputStreamStruct
	 * 		the {@link OutputStreamStruct} to create an AbstractDualStreamStruct from
	 */
	protected AbstractDualStreamStructImpl(final boolean interactive, final InputStreamStruct inputStreamStruct, final OutputStreamStruct outputStreamStruct) {
		super(interactive, getElementType(inputStreamStruct, outputStreamStruct));
		this.inputStreamStruct = inputStreamStruct;
		this.outputStreamStruct = outputStreamStruct;
	}

	/**
	 * This method is used to retrieve the element type for object construction.
	 *
	 * @param inputStreamStruct
	 * 		the {@link InputStreamStruct} to create an AbstractDualStreamStruct from
	 * @param outputStreamStruct
	 * 		the {@link OutputStreamStruct} to create an AbstractDualStreamStruct from
	 *
	 * @return the element type for object construction
	 */
	private static LispStruct getElementType(final InputStreamStruct inputStreamStruct, final OutputStreamStruct outputStreamStruct) {
		if (inputStreamStruct == null) {
			throw new ErrorException("Provided Input Stream must not be null.");
		}
		if (outputStreamStruct == null) {
			throw new ErrorException("Provided Output Stream must not be null.");
		}

		final LispStruct inType = inputStreamStruct.getElementType();
		final LispStruct outType = outputStreamStruct.getElementType();

		return inType.typep(outType).toJavaPBoolean() ? inType : ListStruct.toLispList(CommonLispSymbols.AND, inType, outType);
	}

	/**
	 * Getter for {@link #inputStreamStruct} property.
	 *
	 * @return {@link #inputStreamStruct} property
	 */
	@Override
	public InputStreamStruct getInputStreamStruct() {
		return inputStreamStruct;
	}

	/**
	 * Getter for {@link #outputStreamStruct} property.
	 *
	 * @return {@link #outputStreamStruct} property
	 */
	@Override
	public OutputStreamStruct getOutputStreamStruct() {
		return outputStreamStruct;
	}

	@Override
	public void writeChar(final int aChar) {
		outputStreamStruct.writeChar(aChar);
	}

	@Override
	public void writeByte(final int aByte) {
		outputStreamStruct.writeByte(aByte);
	}

	@Override
	public void writeString(final String outputString, final int start, final int end) {
		outputStreamStruct.writeString(outputString, start, end);
	}

	@Override
	public void clearOutput() {
		outputStreamStruct.clearOutput();
	}

	@Override
	public void finishOutput() {
		outputStreamStruct.finishOutput();
	}

	@Override
	public void forceOutput() {
		outputStreamStruct.forceOutput();
	}

	@Override
	public void clearInput() {
		inputStreamStruct.clearInput();
	}

	@Override
	public boolean listen() {
		return inputStreamStruct.listen();
	}

	@Override
	public Long fileLength() {
		throw new StreamErrorException(StreamUtils.OPERATION_ONLY_FILE_STREAM, this);
	}

	@Override
	public boolean isStartOfLine() {
		return outputStreamStruct.isStartOfLine();
	}
}
