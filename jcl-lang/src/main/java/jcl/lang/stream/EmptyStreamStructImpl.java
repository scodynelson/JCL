/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.stream;

import jcl.lang.EmptyStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.condition.exception.StreamErrorException;
import jcl.type.CharacterType;
import jcl.type.StreamType;

/**
 * The {@link EmptyStreamStructImpl} is the object representation of an empty reading and writing system level Lisp stream.
 */
public final class EmptyStreamStructImpl extends AbstractNativeStreamStructImpl implements EmptyStreamStruct {

	/**
	 * Singleton instance of the {@link EmptyStreamStructImpl} Lisp stream.
	 */
	public static final EmptyStreamStructImpl INSTANCE = new EmptyStreamStructImpl();

	/**
	 * Private constructor.
	 */
	private EmptyStreamStructImpl() {
		super(StreamType.INSTANCE, false, CharacterType.INSTANCE);
	}

	@Override
	public ReadPeekResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		throw new StreamErrorException(StreamUtils.OPERATION_NOT_SUPPORTED_FOR_EMPTY_STREAM, this);
	}

	@Override
	public ReadPeekResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		throw new StreamErrorException(StreamUtils.OPERATION_NOT_SUPPORTED_FOR_EMPTY_STREAM, this);
	}

	@Override
	public ReadPeekResult peekChar(final PeekType peekType, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		throw new StreamErrorException(StreamUtils.OPERATION_NOT_SUPPORTED_FOR_EMPTY_STREAM, this);
	}

	@Override
	public Integer unreadChar(final Integer codePoint) {
		throw new StreamErrorException(StreamUtils.OPERATION_NOT_SUPPORTED_FOR_EMPTY_STREAM, this);
	}

	@Override
	public void clearInput() {
		// Do nothing.
	}

	@Override
	public void writeChar(final int aChar) {
		throw new StreamErrorException(StreamUtils.OPERATION_NOT_SUPPORTED_FOR_EMPTY_STREAM, this);
	}

	@Override
	public void writeByte(final int aByte) {
		throw new StreamErrorException(StreamUtils.OPERATION_ONLY_BINARY_STREAM, this);
	}

	@Override
	public void writeString(final String outputString, final int start, final int end) {
		throw new StreamErrorException(StreamUtils.OPERATION_NOT_SUPPORTED_FOR_EMPTY_STREAM, this);
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
		return false;
	}

	@Override
	public Long fileLength() {
		throw new StreamErrorException(StreamUtils.OPERATION_NOT_SUPPORTED_FOR_EMPTY_STREAM, this);
	}

	@Override
	public Long filePosition(final Long filePosition) {
		throw new StreamErrorException(StreamUtils.OPERATION_NOT_SUPPORTED_FOR_EMPTY_STREAM, this);
	}
}
