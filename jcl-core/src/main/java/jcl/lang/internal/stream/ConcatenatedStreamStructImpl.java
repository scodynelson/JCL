/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal.stream;

import java.util.ArrayDeque;
import java.util.Deque;

import jcl.lang.BooleanStruct;
import jcl.lang.ConcatenatedStreamStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.TStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.condition.exception.EndOfFileException;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.stream.PeekType;
import jcl.lang.stream.ReadPeekResult;

/**
 * The {@link ConcatenatedStreamStructImpl} is the object representation of a Lisp 'concatenated-stream' type.
 */
public final class ConcatenatedStreamStructImpl extends StreamStructImpl implements ConcatenatedStreamStruct {

	/**
	 * This {@link InputStreamStruct}s in the ConcatenatedStreamStruct.
	 */
	private final Deque<InputStreamStruct> inputStreamStructs;

	/**
	 * Public constructor.
	 *
	 * @param inputStreamStructs
	 * 		the {@link InputStreamStruct}s to create a ConcatenatedStreamStruct from
	 */
	public ConcatenatedStreamStructImpl(final Deque<InputStreamStruct> inputStreamStructs) {
		this(false, inputStreamStructs);
	}

	/**
	 * Public constructor.
	 *
	 * @param interactive
	 * 		whether or not the struct created is 'interactive'
	 * @param inputStreamStructs
	 * 		the {@link InputStreamStruct}s to create a ConcatenatedStreamStruct from
	 */
	public ConcatenatedStreamStructImpl(final boolean interactive, final Deque<InputStreamStruct> inputStreamStructs) {
		super(interactive, getElementType2(inputStreamStructs));
		this.inputStreamStructs = new ArrayDeque<>(inputStreamStructs);
	}

	/**
	 * Used to retrieve the element type for object construction.
	 *
	 * @param inputStreamStructs
	 * 		the {@link InputStreamStruct}s to create a ConcatenatedStreamStruct from
	 *
	 * @return the element type for object construction
	 */
	private static LispStruct getElementType2(final Deque<InputStreamStruct> inputStreamStructs) {
		if (inputStreamStructs == null) {
			throw new ErrorException("Provided Input Stream List must not be null.");
		}
		return getElementType3(inputStreamStructs);
	}

	/**
	 * Used to retrieve the element type for object construction.
	 *
	 * @param inputStreamStructs
	 * 		the {@link InputStreamStruct}s to create a ConcatenatedStreamStruct from
	 *
	 * @return the element type for object construction
	 */
	private static LispStruct getElementType3(final Deque<InputStreamStruct> inputStreamStructs) {
		if (inputStreamStructs.isEmpty()) {
			return CommonLispSymbols.T;
		}

		final InputStreamStruct last = inputStreamStructs.getLast();
		return last.getElementType();
	}

	@Override
	public Deque<InputStreamStruct> getInputStreamStructs() {
		return inputStreamStructs;
	}

	@Override
	public ReadPeekResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		while (true) {
			if (inputStreamStructs.isEmpty()) {
				if (eofErrorP) {
					throw new EndOfFileException(StreamUtils.END_OF_FILE_REACHED, this);
				} else {
					return new ReadPeekResult(eofValue);
				}
			}

			final InputStreamStruct first = inputStreamStructs.getFirst();

			final ReadPeekResult readResult = first.readChar(false, eofValue, false);
			if (readResult.isEof()) {
				inputStreamStructs.removeFirst();
			} else {
				return readResult;
			}
		}
	}

	@Override
	public ReadPeekResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		while (true) {
			if (inputStreamStructs.isEmpty()) {
				if (eofErrorP) {
					throw new EndOfFileException(StreamUtils.END_OF_FILE_REACHED, this);
				} else {
					return new ReadPeekResult(eofValue);
				}
			}

			final InputStreamStruct first = inputStreamStructs.getFirst();

			final ReadPeekResult readResult = first.readByte(false, eofValue);
			if (readResult.isEof()) {
				inputStreamStructs.removeFirst();
			} else {
				return readResult;
			}
		}
	}

	@Override
	public ReadPeekResult peekChar(final PeekType peekType, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		if (inputStreamStructs.isEmpty()) {
			if (eofErrorP) {
				throw new EndOfFileException(StreamUtils.END_OF_FILE_REACHED, this);
			} else {
				return new ReadPeekResult(eofValue);
			}
		}

		final InputStreamStruct first = inputStreamStructs.getFirst();
		return first.peekChar(peekType, eofErrorP, eofValue, recursiveP);
	}

	@Override
	public Integer unreadChar(final Integer codePoint) {
		if (inputStreamStructs.isEmpty()) {
			return null;
		}

		final InputStreamStruct first = inputStreamStructs.getFirst();
		return first.unreadChar(codePoint);
	}

	@Override
	public void clearInput() {
		if (!inputStreamStructs.isEmpty()) {
			final InputStreamStruct first = inputStreamStructs.getFirst();
			first.clearInput();
		}
	}

	@Override
	public boolean listen() {
		if (inputStreamStructs.isEmpty()) {
			return false;
		}

		while (true) {
			final InputStreamStruct first = inputStreamStructs.getFirst();
			final boolean listen = first.listen();
			if (listen) {
				return true;
			}

			inputStreamStructs.removeFirst();
			if (inputStreamStructs.isEmpty()) {
				return false;
			}
		}
	}

	@Override
	public LispStruct getElementType() {
		return getElementType3(inputStreamStructs);
	}

	@Override
	public Long fileLength() {
		if (inputStreamStructs.isEmpty()) {
			return 0L;
		}

		final InputStreamStruct last = inputStreamStructs.getLast();
		return last.fileLength();
	}

	@Override
	public Long filePosition(final Long filePosition) {
		if (inputStreamStructs.isEmpty()) {
			return 0L;
		}

		final InputStreamStruct last = inputStreamStructs.getLast();
		return last.filePosition(filePosition);
	}

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
