/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang;

/**
 * The {@link StreamStruct} is the representation for all Lisp 'stream' types.
 */
public interface StreamStruct extends LispStruct {

	/**
	 * Closes the stream, possibly cleaning up side effects depending on the provided {@code abort} value.
	 *
	 * @param abort
	 * 		whether or not to clean up any side effects of the stream
	 *
	 * @return whether or not the stream was closed or modified in any way
	 */
	BooleanStruct close(final BooleanStruct abort);

	/**
	 * Returns the element type of the stream.
	 *
	 * @return the element type of the stream
	 */
	LispStruct streamElementType();

	/**
	 * Returns whether or not the stream is interactive.
	 *
	 * @return whether or not the stream is interactive
	 */
	BooleanStruct interactiveStreamP();

	/**
	 * Sets the stream interactive attribute to the value provided.
	 *
	 * @param interactive
	 * 		the new value of the stream interactive attribute
	 */
	void setInteractive(final boolean interactive);

	/**
	 * Returns whether or not the stream is open.
	 *
	 * @return whether or not the stream is open
	 */
	BooleanStruct openStreamP();

	/**
	 * Returns whether or not the stream is closed.
	 *
	 * @return whether or not the stream is closed
	 */
	BooleanStruct closedStreamP();

	/**
	 * Returns whether or not the stream is an input stream.
	 *
	 * @return whether or not the stream is an input stream
	 */
	default BooleanStruct inputStreamP() {
		return NILStruct.INSTANCE;
	}

	/**
	 * Returns whether or not the stream is an output stream.
	 *
	 * @return whether or not the stream is output stream
	 */
	default BooleanStruct outputStreamP() {
		return NILStruct.INSTANCE;
	}

	/**
	 * Returns the length of the stream if it is a {@link FileStreamStruct}.
	 *
	 * @return the length of the stream
	 */
	LispStruct fileLength();

	/**
	 * Returns the current position in the stream.
	 *
	 * @return the current position in the stream
	 */
	LispStruct filePosition();

	/**
	 * Changes the current position in the stream to the provided {@link IntegerStruct}.
	 *
	 * @param position
	 * 		the new position the stream position will be set to
	 *
	 * @return T if the position was changed; NIL otherwise
	 */
	BooleanStruct filePosition(final IntegerStruct position);

	/**
	 * Returns the current line number (based on the number of newline characters read or written) in the stream.
	 *
	 * @return the current line number (based on the number of newline characters read or written) in the stream
	 */
	IntegerStruct lineNumber();
}
