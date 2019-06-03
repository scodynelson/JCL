/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal.stream;

import jcl.lang.BooleanStruct;
import jcl.lang.CharacterStreamStruct;
import jcl.lang.EmptyStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.condition.exception.StreamErrorException;
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.stream.ReadCharResult;

/**
 * The {@link EmptyStreamStructImpl} is the object representation of an empty reading and writing Lisp stream.
 */
public final class EmptyStreamStructImpl extends StreamStructImpl implements EmptyStreamStruct {

	private static final String OPERATION_NOT_SUPPORTED_FOR_EMPTY_STREAM = "Operation not supported for EmptyStream.";

	/**
	 * Singleton instance of the {@link EmptyStreamStructImpl} Lisp stream.
	 */
	public static final EmptyStreamStructImpl INSTANCE = new EmptyStreamStructImpl();

	/**
	 * Private constructor.
	 */
	private EmptyStreamStructImpl() {
		super(CommonLispSymbols.CHARACTER);
	}

	/*
	INPUT-STREAM-STRUCT
	 */

	@Override
	public ReadCharResult readChar(final boolean eofErrorP, final LispStruct eofValue) {
		throw new StreamErrorException(OPERATION_NOT_SUPPORTED_FOR_EMPTY_STREAM, this);
	}

	@Override
	public ReadCharResult readCharNoHang(final boolean eofErrorP, final LispStruct eofValue) {
		throw new StreamErrorException(OPERATION_NOT_SUPPORTED_FOR_EMPTY_STREAM, this);
	}

	@Override
	public ReadCharResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		throw new StreamErrorException(OPERATION_NOT_SUPPORTED_FOR_EMPTY_STREAM, this);
	}

	@Override
	public Integer unreadChar(final Integer codePoint) {
		throw new StreamErrorException(OPERATION_NOT_SUPPORTED_FOR_EMPTY_STREAM, this);
	}

	@Override
	public BooleanStruct listen() {
		return NILStruct.INSTANCE;
	}

	/*
	OUTPUT-STREAM-STRUCT
	 */

	@Override
	public void writeChar(final int codePoint) {
		throw new StreamErrorException(OPERATION_NOT_SUPPORTED_FOR_EMPTY_STREAM, this);
	}

	@Override
	public void writeByte(final int aByte) {
		throw new StreamErrorException(CharacterStreamStruct.OPERATION_UNSUPPORTED, this);
	}

	@Override
	public void writeString(final String outputString) {
		throw new StreamErrorException(OPERATION_NOT_SUPPORTED_FOR_EMPTY_STREAM, this);
	}

	@Override
	public void writeLine(final String outputString) {
		throw new StreamErrorException(OPERATION_NOT_SUPPORTED_FOR_EMPTY_STREAM, this);
	}

	@Override
	public BooleanStruct freshLine() {
		throw new StreamErrorException(OPERATION_NOT_SUPPORTED_FOR_EMPTY_STREAM, this);
	}

	@Override
	public BooleanStruct terpri() {
		throw new StreamErrorException(OPERATION_NOT_SUPPORTED_FOR_EMPTY_STREAM, this);
	}
}
