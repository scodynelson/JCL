/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal.stream;

import java.util.ArrayDeque;
import java.util.Deque;

import jcl.lang.EchoStreamStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.OutputStreamStruct;
import jcl.lang.condition.exception.EndOfFileException;
import jcl.lang.stream.PeekType;
import jcl.lang.stream.ReadPeekResult;
import jcl.type.EchoStreamType;

/**
 * The {@link EchoStreamStructImpl} is the object representation of a Lisp 'echo-stream' type.
 */
public final class EchoStreamStructImpl extends AbstractDualStreamStructImpl implements EchoStreamStruct {

	/**
	 * The {@link Integer} tokens that have been unread so far.
	 */
	private final Deque<Integer> unreadTokens = new ArrayDeque<>();

	/**
	 * Public constructor.
	 *
	 * @param inputStreamStruct
	 * 		the {@link InputStreamStruct} to create a EchoStreamStruct from
	 * @param outputStreamStruct
	 * 		the {@link OutputStreamStruct} to create a EchoStreamStruct from
	 */
	private EchoStreamStructImpl(final InputStreamStruct inputStreamStruct, final OutputStreamStruct outputStreamStruct) {
		this(false, inputStreamStruct, outputStreamStruct);
	}

	/**
	 * Public constructor.
	 *
	 * @param interactive
	 * 		whether or not the struct created is 'interactive'
	 * @param inputStreamStruct
	 * 		the {@link InputStreamStruct} to create a EchoStreamStruct from
	 * @param outputStreamStruct
	 * 		the {@link OutputStreamStruct} to create a EchoStreamStruct from
	 */
	private EchoStreamStructImpl(final boolean interactive, final InputStreamStruct inputStreamStruct, final OutputStreamStruct outputStreamStruct) {
		super(EchoStreamType.INSTANCE, interactive, inputStreamStruct, outputStreamStruct);
	}

	public static EchoStreamStructImpl valueOf(final InputStreamStruct inputStreamStruct, final OutputStreamStruct outputStreamStruct) {
		return new EchoStreamStructImpl(inputStreamStruct, outputStreamStruct);
	}

	public static EchoStreamStructImpl valueOf(final boolean interactive, final InputStreamStruct inputStreamStruct, final OutputStreamStruct outputStreamStruct) {
		return new EchoStreamStructImpl(interactive, inputStreamStruct, outputStreamStruct);
	}

	@Override
	public ReadPeekResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		if (!unreadTokens.isEmpty()) {
			final Integer lastUnread = unreadTokens.getFirst();
			return new ReadPeekResult(lastUnread);
		}

		final ReadPeekResult readResult = inputStreamStruct.readChar(false, eofValue, false);

		if (readResult.isEof()) {
			if (eofErrorP) {
				throw new EndOfFileException(StreamUtils.END_OF_FILE_REACHED, this);
			} else {
				return readResult;
			}
		} else {
			final int readChar = readResult.getResult();
			outputStreamStruct.writeChar(readChar);
			return readResult;
		}
	}

	@Override
	public ReadPeekResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		if (!unreadTokens.isEmpty()) {
			final Integer lastUnread = unreadTokens.getFirst();
			return new ReadPeekResult(lastUnread);
		}

		final ReadPeekResult readResult = inputStreamStruct.readByte(false, eofValue);

		if (readResult.isEof()) {
			if (eofErrorP) {
				throw new EndOfFileException(StreamUtils.END_OF_FILE_REACHED, this);
			} else {
				return readResult;
			}
		} else {
			final int readByte = readResult.getResult();
			outputStreamStruct.writeByte(readByte);
			return readResult;
		}
	}

	@Override
	public ReadPeekResult peekChar(final PeekType peekType, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		if (unreadTokens.isEmpty()) {
			final ReadPeekResult readResult = inputStreamStruct.readChar(eofErrorP, eofValue, recursiveP);

			if (readResult.isEof()) {
				return new ReadPeekResult(readResult.getEofValue());
			} else {
				final int peekedChar = readResult.getResult();
				outputStreamStruct.writeChar(peekedChar);
				return new ReadPeekResult(peekedChar);
			}
		} else {
			final Integer peekedChar = unreadTokens.removeFirst();
			return new ReadPeekResult(peekedChar);
		}
	}

	@Override
	public Integer unreadChar(final Integer codePoint) {
		unreadTokens.addFirst(codePoint);
		return codePoint;
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
