/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang;

import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.statics.CommonLispSymbols;

/**
 * The {@link StreamStruct} is the representation for all Lisp 'stream' types.
 */
public interface StreamStruct extends LispStruct {

	/**
	 * Closes the stream.
	 *
	 * @return whether or not the stream was closed or modified in any way
	 */
	boolean close();

	default boolean close(final boolean abort) {
		return close();
	}

	default BooleanStruct close (final BooleanStruct abort) {
		final boolean wasClosed = close(abort.toJavaPBoolean());
		return BooleanStruct.toLispBoolean(wasClosed);
	}

	/**
	 * Returns the element type of the stream.
	 *
	 * @return the element type of the stream
	 */
	LispStruct getElementType();

	default LispStruct streamElementType() {
		return getElementType();
	}

	/**
	 * Returns whether or not the stream is interactive.
	 *
	 * @return whether or not the stream is interactive
	 */
	boolean isInteractive();

	void setInteractive(final boolean interactive);

	default BooleanStruct interactiveStreamP() {
		return BooleanStruct.toLispBoolean(isInteractive());
	}

	/**
	 * Returns whether or not the stream is open.
	 *
	 * @return whether or not the stream is open
	 */
	boolean isOpen();

	/**
	 * Returns whether or not the stream is closed.
	 *
	 * @return whether or not the stream is closed
	 */
	boolean isClosed();

	default BooleanStruct openStreamP() {
		return BooleanStruct.toLispBoolean(isOpen());
	}

	/**
	 * Returns the length of the stream if it is a {@link FileStreamStruct}.
	 *
	 * @return the length of the stream
	 */
	Long fileLength();

	default LispStruct fileLength1() {
		// TODO: Fix method name
		final Long fileLength = fileLength();
		if (fileLength == null) {
			return NILStruct.INSTANCE;
		} else {
			return IntegerStruct.toLispInteger(fileLength);
		}
	}

	/**
	 * Returns the current position in the stream if it is a {@link FileStreamStruct}.
	 *
	 * @param filePosition
	 * 		if not null, the current stream position will be set to this value
	 *
	 * @return the current position in the stream
	 */
	Long filePosition(Long filePosition);

	default LispStruct filePosition1(final LispStruct positionParam) {
		// TODO: Fix method name
		final Long position;
		if (positionParam instanceof IntegerStruct) {
			position = ((IntegerStruct) positionParam).toJavaPLong();
		} else if (CommonLispSymbols.START_KEYWORD.eq(positionParam)) {
			position = 0L;
		} else if (CommonLispSymbols.END_KEYWORD.eq(positionParam)) {
			position = fileLength();
		} else if (NILStruct.INSTANCE.eq(positionParam)) {
			position = null;
		} else {
			throw new TypeErrorException("UNCAUGHT TYPE ERROR.");
		}

		if (position == null) {
			final Long currentPosition = filePosition(null);
			return IntegerStruct.toLispInteger(currentPosition);
		} else {
			final Long newPosition = filePosition(position);
			return BooleanStruct.toLispBoolean(newPosition != null);
		}
	}

	default LispStruct fileStringLength(final LispStruct object) {
		if (object instanceof CharacterStruct) {
			return IntegerStruct.ONE;
		} else if (object instanceof StringStruct) {
			return ((StringStruct) object).length();
		} else {
			throw new TypeErrorException("UNCAUGHT TYPE ERROR.");
		}
	}

	default boolean isInputStream() {
		return false;
	}

	default BooleanStruct inputStreamP() {
		return BooleanStruct.toLispBoolean(isInputStream());
	}

	default boolean isOutputStream() {
		return false;
	}

	default BooleanStruct outputStreamP() {
		return BooleanStruct.toLispBoolean(isOutputStream());
	}

	Long lineNumber();
}
