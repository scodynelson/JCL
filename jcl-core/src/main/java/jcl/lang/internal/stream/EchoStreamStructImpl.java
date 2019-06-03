/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal.stream;

import java.util.ArrayDeque;
import java.util.Deque;

import jcl.lang.BooleanStruct;
import jcl.lang.EchoStreamStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.OutputStreamStruct;
import jcl.lang.TStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.condition.exception.EndOfFileException;
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.stream.ReadCharResult;

/**
 * The {@link EchoStreamStructImpl} is the object representation of a Lisp 'echo-stream' type.
 */
public final class EchoStreamStructImpl extends AbstractDualStreamStructImpl implements EchoStreamStruct {

	/**
	 * The {@link Integer} tokens that have been unread so far.
	 */
	private final Deque<Integer> unreadTokens = new ArrayDeque<>();

	/**
	 * Public constructor, initializing the provided {@link InputStreamStruct} and {@link OutputStreamStruct}.
	 *
	 * @param inputStreamStruct
	 * 		the {@link InputStreamStruct} to initialize
	 * @param outputStreamStruct
	 * 		the {@link OutputStreamStruct} to initialize
	 */
	public EchoStreamStructImpl(final InputStreamStruct inputStreamStruct,
	                            final OutputStreamStruct outputStreamStruct) {
		super(inputStreamStruct, outputStreamStruct);
	}

	/*
	ECHO-STREAM-STRUCT
	 */

	@Override
	public InputStreamStruct echoStreamInputStream() {
		return inputStreamStruct;
	}

	@Override
	public OutputStreamStruct echoStreamOutputStream() {
		return outputStreamStruct;
	}

	/*
	INPUT-STREAM-STRUCT
	 */

	@Override
	public ReadCharResult readChar(final boolean eofErrorP, final LispStruct eofValue) {
		if (!unreadTokens.isEmpty()) {
			final Integer lastUnread = unreadTokens.getFirst();
			return new ReadCharResult(lastUnread);
		}

		final ReadCharResult readResult = inputStreamStruct.readChar(false, eofValue);

		if (readResult.isEof()) {
			if (eofErrorP) {
				throw new EndOfFileException(this);
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
	public ReadCharResult readCharNoHang(final boolean eofErrorP, final LispStruct eofValue) {
		if (!unreadTokens.isEmpty()) {
			final Integer lastUnread = unreadTokens.getFirst();
			return new ReadCharResult(lastUnread);
		}

		final ReadCharResult readResult = inputStreamStruct.readCharNoHang(false, eofValue);

		if (readResult.isEof()) {
			if (eofErrorP) {
				throw new EndOfFileException(this);
			} else {
				return readResult;
			}
		} else {
			final int readChar = readResult.getResult();

			// TODO: probably not the most efficient implementation
			if (readChar != -5) {
				outputStreamStruct.writeChar(readChar);
			}
			return readResult;
		}
	}

	@Override
	public ReadCharResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		if (!unreadTokens.isEmpty()) {
			final Integer lastUnread = unreadTokens.getFirst();
			return new ReadCharResult(lastUnread);
		}

		final ReadCharResult readResult = inputStreamStruct.readByte(false, eofValue);

		if (readResult.isEof()) {
			if (eofErrorP) {
				throw new EndOfFileException(this);
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
	public Integer unreadChar(final Integer codePoint) {
		unreadTokens.addFirst(codePoint);
		return codePoint;
	}

	/*
	LISP-STRUCT
	 */

	@Override
	public LispStruct typeOf() {
		return CommonLispSymbols.ECHO_STREAM;
	}

	@Override
	public ClassStruct classOf() {
		return BuiltInClassStruct.ECHO_STREAM;
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if (typeSpecifier == CommonLispSymbols.ECHO_STREAM) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.ECHO_STREAM) {
			return TStruct.INSTANCE;
		}
		return super.typep(typeSpecifier);
	}
}
