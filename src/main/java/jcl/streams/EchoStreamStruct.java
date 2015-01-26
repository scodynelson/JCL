/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams;

import jcl.LispStruct;
import jcl.conditions.exceptions.EndOfFileException;
import jcl.types.EchoStream;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.ArrayDeque;
import java.util.Deque;

/**
 * The {@link EchoStreamStruct} is the object representation of a Lisp 'echo-stream' type.
 */
public class EchoStreamStruct extends AbstractDualStreamStruct {

	private static final long serialVersionUID = 5140093388290527648L;

	/**
	 * The {@link Integer} tokens that have been unread so far.
	 */
	private final Deque<Integer> unreadTokens = new ArrayDeque<>();

	/**
	 * Public constructor.
	 *
	 * @param inputStream
	 * 		the {@link InputStream} to create a EchoStreamStruct from
	 * @param outputStream
	 * 		the {@link OutputStream} to create a EchoStreamStruct from
	 */
	public EchoStreamStruct(final InputStream inputStream, final OutputStream outputStream) {
		this(false, inputStream, outputStream);
	}

	/**
	 * Public constructor.
	 *
	 * @param interactive
	 * 		whether or not the struct created is 'interactive'
	 * @param inputStream
	 * 		the {@link InputStream} to create a EchoStreamStruct from
	 * @param outputStream
	 * 		the {@link OutputStream} to create a EchoStreamStruct from
	 */
	public EchoStreamStruct(final boolean interactive, final InputStream inputStream, final OutputStream outputStream) {
		super(EchoStream.INSTANCE, interactive, inputStream, outputStream);
	}

	@Override
	public ReadPeekResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		if (!unreadTokens.isEmpty()) {
			final Integer lastUnread = unreadTokens.getFirst();
			return new ReadPeekResult(lastUnread);
		}

		final ReadPeekResult readResult = inputStream.readChar(false, eofValue, false);

		if (readResult.isEof()) {
			if (eofErrorP) {
				throw new EndOfFileException(StreamUtils.END_OF_FILE_REACHED);
			} else {
				return readResult;
			}
		} else {
			final int readChar = readResult.getResult();
			outputStream.writeChar(readChar);
			return readResult;
		}
	}

	@Override
	public ReadPeekResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		if (!unreadTokens.isEmpty()) {
			final Integer lastUnread = unreadTokens.getFirst();
			return new ReadPeekResult(lastUnread);
		}

		final ReadPeekResult readResult = inputStream.readByte(false, eofValue);

		if (readResult.isEof()) {
			if (eofErrorP) {
				throw new EndOfFileException(StreamUtils.END_OF_FILE_REACHED);
			} else {
				return readResult;
			}
		} else {
			final int readByte = readResult.getResult();
			outputStream.writeByte(readByte);
			return readResult;
		}
	}

	@Override
	public ReadPeekResult peekChar(final PeekType peekType, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		if (unreadTokens.isEmpty()) {
			final ReadPeekResult readResult = inputStream.readChar(eofErrorP, eofValue, recursiveP);

			if (readResult.isEof()) {
				return new ReadPeekResult(readResult.getEofValue());
			} else {
				final int peekedChar = readResult.getResult();
				outputStream.writeChar(peekedChar);
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
	protected String getPrintableObjectProperties() {
		return " input " + inputStream.printStruct() + ", output " + outputStream.printStruct();
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
