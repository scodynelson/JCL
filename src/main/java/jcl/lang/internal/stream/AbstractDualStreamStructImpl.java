/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal.stream;

import jcl.lang.BooleanStruct;
import jcl.lang.IOStreamStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.OutputStreamStruct;
import jcl.lang.statics.CommonLispSymbols;

/**
 * The {@link AbstractDualStreamStructImpl} is an abstraction for dual stream types.
 */
abstract class AbstractDualStreamStructImpl extends StreamStructImpl implements IOStreamStruct {

	/**
	 * The {@link InputStreamStruct} for reading input.
	 */
	protected final InputStreamStruct inputStreamStruct;

	/**
	 * The {@link OutputStreamStruct} for writing output.
	 */
	protected final OutputStreamStruct outputStreamStruct;

	/**
	 * Protected constructor, initializing the {@link #inputStreamStruct} and {@link #outputStreamStruct}.
	 *
	 * @param inputStreamStruct
	 * 		the {@link InputStreamStruct} to initialize
	 * @param outputStreamStruct
	 * 		the {@link OutputStreamStruct} to initialize
	 */
	protected AbstractDualStreamStructImpl(final InputStreamStruct inputStreamStruct,
	                                       final OutputStreamStruct outputStreamStruct) {
		super(getElementType(inputStreamStruct, outputStreamStruct));
		this.inputStreamStruct = inputStreamStruct;
		this.outputStreamStruct = outputStreamStruct;
	}

	/**
	 * Retrieves the element type for this dual-stream based on the provided {@link InputStreamStruct} and
	 * {@link OutputStreamStruct}.
	 *
	 * @param inputStreamStruct
	 * 		the {@link InputStreamStruct} used to find the stream element-type
	 * @param outputStreamStruct
	 * 		the {@link OutputStreamStruct} used to find the stream element-type
	 *
	 * @return the element type for this dual-stream
	 */
	private static LispStruct getElementType(final InputStreamStruct inputStreamStruct,
	                                         final OutputStreamStruct outputStreamStruct) {
		final LispStruct inType = inputStreamStruct.streamElementType();
		final LispStruct outType = outputStreamStruct.streamElementType();

		final boolean isSameTypeStream = inType.typep(outType).toJavaPBoolean();
		return isSameTypeStream ? inType : ListStruct.toLispList(CommonLispSymbols.AND, inType, outType);
	}

	/*
	INPUT-STREAM-STRUCT
	 */

	@Override
	public LispStruct clearInput() {
		return inputStreamStruct.clearInput();
	}

	@Override
	public BooleanStruct listen() {
		return inputStreamStruct.listen();
	}

	/*
	OUTPUT-STREAM-STRUCT
	 */

	@Override
	public BooleanStruct close(final BooleanStruct abort) {
		if (abort.toJavaPBoolean()) {
			clearOutput();
		} else {
			forceOutput();
		}
		return super.close(abort);
	}

	@Override
	public void writeChar(final int codePoint) {
		outputStreamStruct.writeChar(codePoint);
	}

	@Override
	public void writeByte(final int aByte) {
		outputStreamStruct.writeByte(aByte);
	}

	@Override
	public void writeString(final String outputString) {
		outputStreamStruct.writeString(outputString);
	}

	@Override
	public void writeLine(final String outputString) {
		outputStreamStruct.writeLine(outputString);
	}

	@Override
	public BooleanStruct freshLine() {
		return outputStreamStruct.freshLine();
	}

	@Override
	public BooleanStruct terpri() {
		return outputStreamStruct.terpri();
	}

	@Override
	public LispStruct clearOutput() {
		return outputStreamStruct.clearOutput();
	}

	@Override
	public LispStruct finishOutput() {
		return outputStreamStruct.finishOutput();
	}

	@Override
	public LispStruct forceOutput() {
		return outputStreamStruct.forceOutput();
	}

	/*
	LISP-STRUCT
	 */

	@Override
	public String toString() {
		return "#<" + typeOf() + " input " + inputStreamStruct + ", output " + outputStreamStruct + '>';
	}
}
