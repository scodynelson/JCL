/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams;

import jcl.LispStruct;
import jcl.conditions.exceptions.StreamErrorException;
import jcl.types.CharacterType;
import jcl.types.StreamType;

/**
 * The {@link EmptyStreamStruct} is the object representation of an empty reading and writing system level Lisp stream.
 */
public final class EmptyStreamStruct extends AbstractNativeStreamStruct {

	/**
	 * Singleton instance of the {@link EmptyStreamStruct} Lisp stream.
	 */
	public static final EmptyStreamStruct INSTANCE = new EmptyStreamStruct();

	/**
	 * Private constructor.
	 */
	private EmptyStreamStruct() {
		super(StreamType.INSTANCE, false, CharacterType.INSTANCE);
	}

	@Override
	public ReadPeekResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		throw new StreamErrorException(StreamUtils.OPERATION_NOT_SUPPORTED_FOR_EMPTY_STREAM);
	}

	@Override
	public ReadPeekResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		throw new StreamErrorException(StreamUtils.OPERATION_NOT_SUPPORTED_FOR_EMPTY_STREAM);
	}

	@Override
	public ReadPeekResult peekChar(final PeekType peekType, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		throw new StreamErrorException(StreamUtils.OPERATION_NOT_SUPPORTED_FOR_EMPTY_STREAM);
	}

	@Override
	public Integer unreadChar(final Integer codePoint) {
		throw new StreamErrorException(StreamUtils.OPERATION_NOT_SUPPORTED_FOR_EMPTY_STREAM);
	}

	@Override
	public void clearInput() {
		// Do nothing.
	}

	@Override
	public void writeChar(final int aChar) {
		throw new StreamErrorException(StreamUtils.OPERATION_NOT_SUPPORTED_FOR_EMPTY_STREAM);
	}

	@Override
	public void writeByte(final int aByte) {
		throw new StreamErrorException(StreamUtils.OPERATION_ONLY_BINARY_STREAM);
	}

	@Override
	public void writeString(final String outputString, final int start, final int end) {
		throw new StreamErrorException(StreamUtils.OPERATION_NOT_SUPPORTED_FOR_EMPTY_STREAM);
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
		throw new StreamErrorException(StreamUtils.OPERATION_NOT_SUPPORTED_FOR_EMPTY_STREAM);
	}

	@Override
	public Long filePosition(final Long filePosition) {
		throw new StreamErrorException(StreamUtils.OPERATION_NOT_SUPPORTED_FOR_EMPTY_STREAM);
	}
}
