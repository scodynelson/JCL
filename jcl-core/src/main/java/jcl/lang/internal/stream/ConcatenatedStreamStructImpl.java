/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal.stream;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;

import jcl.lang.BooleanStruct;
import jcl.lang.ConcatenatedStreamStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.TStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.condition.exception.EndOfFileException;
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.stream.ReadCharResult;

/**
 * The {@link ConcatenatedStreamStructImpl} is the object representation of a Lisp 'concatenated-stream' type.
 */
public final class ConcatenatedStreamStructImpl extends StreamStructImpl implements ConcatenatedStreamStruct {

	/**
	 * This {@link Deque} of {@link InputStreamStruct} objects where input is accepted from.
	 */
	private final Deque<InputStreamStruct> inputStreamStructs;

	/**
	 * Public constructor, initializing the provided the {@link Deque} of {@link InputStreamStruct} objects.
	 *
	 * @param inputStreamStructs
	 * 		the {@link Deque} of {@link InputStreamStruct} objects to initialize
	 */
	public ConcatenatedStreamStructImpl(final Deque<InputStreamStruct> inputStreamStructs) {
		super(getElementType(inputStreamStructs));
		this.inputStreamStructs = new ArrayDeque<>(inputStreamStructs);
	}

	/**
	 * Retrieves the current element type for the {@link Deque} of {@link InputStreamStruct} objects.
	 *
	 * @param inputStreamStructs
	 * 		the {@link Deque} of {@link InputStreamStruct} objects to retrieve the element type from
	 *
	 * @return the current element type for the {@link Deque} of {@link InputStreamStruct} objects
	 */
	private static LispStruct getElementType(final Deque<InputStreamStruct> inputStreamStructs) {
		if (inputStreamStructs.isEmpty()) {
			return CommonLispSymbols.T;
		}

		final InputStreamStruct last = inputStreamStructs.getLast();
		return last.streamElementType();
	}

	/*
	CONCATENATED-STREAM-STRUCT
	 */

	@Override
	public ListStruct concatenatedStreamStreams() {
		return ListStruct.toLispList(new ArrayList<LispStruct>(inputStreamStructs));
	}

	/*
	INPUT-STREAM-STRUCT
	 */

	@Override
	public ReadCharResult readChar(final boolean eofErrorP, final LispStruct eofValue) {
		while (true) {
			if (inputStreamStructs.isEmpty()) {
				if (eofErrorP) {
					throw new EndOfFileException(this);
				} else {
					return new ReadCharResult(eofValue);
				}
			}

			final InputStreamStruct first = inputStreamStructs.getFirst();

			final ReadCharResult readResult = first.readChar(false, eofValue);
			if (readResult.isEof()) {
				inputStreamStructs.removeFirst();
			} else {
				return readResult;
			}
		}
	}

	@Override
	public ReadCharResult readCharNoHang(final boolean eofErrorP, final LispStruct eofValue) {
		while (true) {
			if (inputStreamStructs.isEmpty()) {
				if (eofErrorP) {
					throw new EndOfFileException(this);
				} else {
					return new ReadCharResult(eofValue);
				}
			}

			final InputStreamStruct first = inputStreamStructs.getFirst();

			final ReadCharResult readResult = first.readCharNoHang(false, eofValue);
			if (readResult.isEof()) {
				inputStreamStructs.removeFirst();
			} else {
				return readResult;
			}
		}
	}

	@Override
	public ReadCharResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		while (true) {
			if (inputStreamStructs.isEmpty()) {
				if (eofErrorP) {
					throw new EndOfFileException(this);
				} else {
					return new ReadCharResult(eofValue);
				}
			}

			final InputStreamStruct first = inputStreamStructs.getFirst();

			final ReadCharResult readResult = first.readByte(false, eofValue);
			if (readResult.isEof()) {
				inputStreamStructs.removeFirst();
			} else {
				return readResult;
			}
		}
	}

	@Override
	public Integer unreadChar(final Integer codePoint) {
		if (inputStreamStructs.isEmpty()) {
			throw new EndOfFileException(this);
		}

		final InputStreamStruct first = inputStreamStructs.getFirst();
		return first.unreadChar(codePoint);
	}

	@Override
	public LispStruct clearInput() {
		if (!inputStreamStructs.isEmpty()) {
			final InputStreamStruct first = inputStreamStructs.getFirst();
			first.clearInput();
		}
		return NILStruct.INSTANCE;
	}

	@Override
	public BooleanStruct listen() {
		if (inputStreamStructs.isEmpty()) {
			return NILStruct.INSTANCE;
		}

		while (true) {
			final InputStreamStruct first = inputStreamStructs.getFirst();
			final BooleanStruct listen = first.listen();
			if (listen.toJavaPBoolean()) {
				return listen;
			}

			inputStreamStructs.removeFirst();
			if (inputStreamStructs.isEmpty()) {
				return NILStruct.INSTANCE;
			}
		}
	}

	/*
	STREAM-STRUCT
	 */

	/**
	 * {@inheritDoc}
	 * <p>
	 * The element type can differ based on the state of the {@link #inputStreamStructs} {@link Deque}.
	 *
	 * @return the current stream-element-type
	 */
	@Override
	public LispStruct streamElementType() {
		return getElementType(inputStreamStructs);
	}

	@Override
	public LispStruct fileLength() {
		if (inputStreamStructs.isEmpty()) {
			return IntegerStruct.ZERO;
		}

		final InputStreamStruct last = inputStreamStructs.getLast();
		return last.fileLength();
	}

	@Override
	public LispStruct filePosition() {
		if (inputStreamStructs.isEmpty()) {
			return IntegerStruct.ZERO;
		}

		final InputStreamStruct last = inputStreamStructs.getLast();
		return last.filePosition();
	}

	/*
	LISP-STRUCT
	 */

	@Override
	public LispStruct typeOf() {
		return CommonLispSymbols.CONCATENATED_STREAM;
	}

	@Override
	public ClassStruct classOf() {
		return BuiltInClassStruct.CONCATENATED_STREAM;
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if (typeSpecifier == CommonLispSymbols.CONCATENATED_STREAM) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.CONCATENATED_STREAM) {
			return TStruct.INSTANCE;
		}
		return super.typep(typeSpecifier);
	}
}
