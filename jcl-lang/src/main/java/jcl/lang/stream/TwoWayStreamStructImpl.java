/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.stream;

import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.OutputStreamStruct;
import jcl.type.TwoWayStreamType;

/**
 * The {@link TwoWayStreamStructImpl} is the object representation of a Lisp 'two-way-stream' type.
 */
public final class TwoWayStreamStructImpl extends AbstractDualStreamStructImpl {

	/**
	 * Public constructor.
	 *
	 * @param inputStreamStruct
	 * 		the {@link InputStreamStruct} to create a TwoWayStreamStruct from
	 * @param outputStreamStruct
	 * 		the {@link OutputStreamStruct} to create a TwoWayStreamStruct from
	 */
	private TwoWayStreamStructImpl(final InputStreamStruct inputStreamStruct, final OutputStreamStruct outputStreamStruct) {
		this(false, inputStreamStruct, outputStreamStruct);
	}

	/**
	 * Public constructor.
	 *
	 * @param interactive
	 * 		whether or not the struct created is 'interactive'
	 * @param inputStreamStruct
	 * 		the {@link InputStreamStruct} to create a TwoWayStreamStruct from
	 * @param outputStreamStruct
	 * 		the {@link OutputStreamStruct} to create a TwoWayStreamStruct from
	 */
	private TwoWayStreamStructImpl(final boolean interactive, final InputStreamStruct inputStreamStruct, final OutputStreamStruct outputStreamStruct) {
		super(TwoWayStreamType.INSTANCE, interactive, inputStreamStruct, outputStreamStruct);
	}

	public static TwoWayStreamStructImpl valueOf(final InputStreamStruct inputStreamStruct, final OutputStreamStruct outputStreamStruct) {
		return new TwoWayStreamStructImpl(inputStreamStruct, outputStreamStruct);
	}

	public static TwoWayStreamStructImpl valueOf(final boolean interactive, final InputStreamStruct inputStreamStruct, final OutputStreamStruct outputStreamStruct) {
		return new TwoWayStreamStructImpl(interactive, inputStreamStruct, outputStreamStruct);
	}

	@Override
	public ReadPeekResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		return inputStreamStruct.readChar(eofErrorP, eofValue, recursiveP);
	}

	@Override
	public ReadPeekResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		return inputStreamStruct.readByte(eofErrorP, eofValue);
	}

	@Override
	public ReadPeekResult peekChar(final PeekType peekType, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		return inputStreamStruct.peekChar(peekType, eofErrorP, eofValue, recursiveP);
	}

	@Override
	public Integer unreadChar(final Integer codePoint) {
		return inputStreamStruct.unreadChar(codePoint);
	}

	@Override
	public Long filePosition(final Long filePosition) {
		return null;
	}

	@Override
	public String toString() {
		final String typeClassName = getType().getClass().getSimpleName().toUpperCase();
		final String printedInputStream = inputStreamStruct.toString();
		final String printedOutputStream = outputStreamStruct.toString();
		return "#<" + typeClassName + " input " + printedInputStream + ", output " + printedOutputStream + '>';
	}
}
