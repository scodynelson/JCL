/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.stream;

import jcl.lang.LispStruct;
import jcl.type.TwoWayStreamType;

/**
 * The {@link TwoWayStreamStruct} is the object representation of a Lisp 'two-way-stream' type.
 */
public final class TwoWayStreamStruct extends AbstractDualStreamStruct {

	/**
	 * Public constructor.
	 *
	 * @param inputStream
	 * 		the {@link InputStream} to create a TwoWayStreamStruct from
	 * @param outputStream
	 * 		the {@link OutputStream} to create a TwoWayStreamStruct from
	 */
	private TwoWayStreamStruct(final InputStream inputStream, final OutputStream outputStream) {
		this(false, inputStream, outputStream);
	}

	/**
	 * Public constructor.
	 *
	 * @param interactive
	 * 		whether or not the struct created is 'interactive'
	 * @param inputStream
	 * 		the {@link InputStream} to create a TwoWayStreamStruct from
	 * @param outputStream
	 * 		the {@link OutputStream} to create a TwoWayStreamStruct from
	 */
	private TwoWayStreamStruct(final boolean interactive, final InputStream inputStream, final OutputStream outputStream) {
		super(TwoWayStreamType.INSTANCE, interactive, inputStream, outputStream);
	}

	public static TwoWayStreamStruct valueOf(final InputStream inputStream, final OutputStream outputStream) {
		return new TwoWayStreamStruct(inputStream, outputStream);
	}

	public static TwoWayStreamStruct valueOf(final boolean interactive, final InputStream inputStream, final OutputStream outputStream) {
		return new TwoWayStreamStruct(interactive, inputStream, outputStream);
	}

	@Override
	public ReadPeekResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		return inputStream.readChar(eofErrorP, eofValue, recursiveP);
	}

	@Override
	public ReadPeekResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		return inputStream.readByte(eofErrorP, eofValue);
	}

	@Override
	public ReadPeekResult peekChar(final PeekType peekType, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		return inputStream.peekChar(peekType, eofErrorP, eofValue, recursiveP);
	}

	@Override
	public Integer unreadChar(final Integer codePoint) {
		return inputStream.unreadChar(codePoint);
	}

	@Override
	public Long filePosition(final Long filePosition) {
		return null;
	}

	@Override
	public String toString() {
		final String typeClassName = getType().getClass().getSimpleName().toUpperCase();
		final String printedInputStream = inputStream.toString();
		final String printedOutputStream = outputStream.toString();
		return "#<" + typeClassName + " input " + printedInputStream + ", output " + printedOutputStream + '>';
	}
}
