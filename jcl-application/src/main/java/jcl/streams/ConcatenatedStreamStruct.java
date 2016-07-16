/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams;

import java.util.ArrayDeque;
import java.util.Deque;

import jcl.LispStruct;
import jcl.LispType;
import jcl.conditions.exceptions.EndOfFileException;
import jcl.conditions.exceptions.ErrorException;
import jcl.types.ConcatenatedStreamType;
import jcl.types.TType;

/**
 * The {@link ConcatenatedStreamStruct} is the object representation of a Lisp 'concatenated-stream' type.
 */
public class ConcatenatedStreamStruct extends StreamStruct implements InputStream {

	/**
	 * This {@link InputStream}s in the ConcatenatedStreamStruct.
	 */
	private final Deque<InputStream> inputStreams;

	/**
	 * Public constructor.
	 *
	 * @param inputStreams
	 * 		the {@link InputStream}s to create a ConcatenatedStreamStruct from
	 */
	public ConcatenatedStreamStruct(final Deque<InputStream> inputStreams) {
		this(false, inputStreams);
	}

	/**
	 * Public constructor.
	 *
	 * @param interactive
	 * 		whether or not the struct created is 'interactive'
	 * @param inputStreams
	 * 		the {@link InputStream}s to create a ConcatenatedStreamStruct from
	 */
	public ConcatenatedStreamStruct(final boolean interactive, final Deque<InputStream> inputStreams) {
		super(ConcatenatedStreamType.INSTANCE, null, null, interactive, getElementType2(inputStreams));
		this.inputStreams = new ArrayDeque<>(inputStreams);
	}

	/**
	 * Used to retrieve the element type for object construction.
	 *
	 * @param inputStreams
	 * 		the {@link InputStream}s to create a ConcatenatedStreamStruct from
	 *
	 * @return the element type for object construction
	 */
	private static LispType getElementType2(final Deque<InputStream> inputStreams) {
		if (inputStreams == null) {
			throw new ErrorException("Provided Input Stream List must not be null.");
		}
		return getElementType3(inputStreams);
	}

	/**
	 * Used to retrieve the element type for object construction.
	 *
	 * @param inputStreams
	 * 		the {@link InputStream}s to create a ConcatenatedStreamStruct from
	 *
	 * @return the element type for object construction
	 */
	private static LispType getElementType3(final Deque<InputStream> inputStreams) {
		if (inputStreams.isEmpty()) {
			return TType.INSTANCE;
		}

		final InputStream last = inputStreams.getLast();
		return last.getElementType();
	}

	public Deque<InputStream> getInputStreams() {
		return inputStreams;
	}

	@Override
	public ReadPeekResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		while (true) {
			if (inputStreams.isEmpty()) {
				if (eofErrorP) {
					throw new EndOfFileException(StreamUtils.END_OF_FILE_REACHED, this);
				} else {
					return new ReadPeekResult(eofValue);
				}
			}

			final InputStream first = inputStreams.getFirst();

			final ReadPeekResult readResult = first.readChar(false, eofValue, false);
			if (readResult.isEof()) {
				inputStreams.removeFirst();
			} else {
				return readResult;
			}
		}
	}

	@Override
	public ReadPeekResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		while (true) {
			if (inputStreams.isEmpty()) {
				if (eofErrorP) {
					throw new EndOfFileException(StreamUtils.END_OF_FILE_REACHED, this);
				} else {
					return new ReadPeekResult(eofValue);
				}
			}

			final InputStream first = inputStreams.getFirst();

			final ReadPeekResult readResult = first.readByte(false, eofValue);
			if (readResult.isEof()) {
				inputStreams.removeFirst();
			} else {
				return readResult;
			}
		}
	}

	@Override
	public ReadPeekResult peekChar(final PeekType peekType, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		if (inputStreams.isEmpty()) {
			if (eofErrorP) {
				throw new EndOfFileException(StreamUtils.END_OF_FILE_REACHED, this);
			} else {
				return new ReadPeekResult(eofValue);
			}
		}

		final InputStream first = inputStreams.getFirst();
		return first.peekChar(peekType, eofErrorP, eofValue, recursiveP);
	}

	@Override
	public Integer unreadChar(final Integer codePoint) {
		if (inputStreams.isEmpty()) {
			return null;
		}

		final InputStream first = inputStreams.getFirst();
		return first.unreadChar(codePoint);
	}

	@Override
	public void clearInput() {
		if (!inputStreams.isEmpty()) {
			final InputStream first = inputStreams.getFirst();
			first.clearInput();
		}
	}

	@Override
	public boolean listen() {
		if (inputStreams.isEmpty()) {
			return false;
		}

		while (true) {
			final InputStream first = inputStreams.getFirst();
			final boolean listen = first.listen();
			if (listen) {
				return true;
			}

			inputStreams.removeFirst();
			if (inputStreams.isEmpty()) {
				return false;
			}
		}
	}

	@Override
	public LispType getElementType() {
		return getElementType3(inputStreams);
	}

	@Override
	public Long fileLength() {
		if (inputStreams.isEmpty()) {
			return 0L;
		}

		final InputStream last = inputStreams.getLast();
		return last.fileLength();
	}

	@Override
	public Long filePosition(final Long filePosition) {
		if (inputStreams.isEmpty()) {
			return 0L;
		}

		final InputStream last = inputStreams.getLast();
		return last.filePosition(filePosition);
	}
}
