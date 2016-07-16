/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.stream;

import jcl.lang.LispStruct;
import jcl.type.LispType;

/**
 * The {@link LispStream} is the representation for all Lisp 'stream' types.
 */
public interface LispStream extends LispStruct {

	/**
	 * Closes the stream.
	 *
	 * @return whether or not the stream was closed or modified in any way
	 */
	boolean close();

	default boolean close(final boolean abort) {
		return close();
	}

	/**
	 * Returns the element type of the stream.
	 *
	 * @return the element type of the stream
	 */
	LispType getElementType();

	/**
	 * Returns whether or not the stream is interactive.
	 *
	 * @return whether or not the stream is interactive
	 */
	boolean isInteractive();

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

	/**
	 * Returns the length of the stream if it is a {@link FileStreamStruct}.
	 *
	 * @return the length of the stream
	 */
	Long fileLength();

	/**
	 * Returns the current position in the stream if it is a {@link FileStreamStruct}.
	 *
	 * @param filePosition
	 * 		if not null, the current stream position will be set to this value
	 *
	 * @return the current position in the stream
	 */
	Long filePosition(Long filePosition);

	default boolean isInputStream() {
		return false;
	}

	default boolean isOutputStream() {
		return false;
	}

	Long lineNumber();
}
