/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams;

/**
 * The {@link OutputStream} is the representation for all Lisp output 'stream' types.
 */
public interface OutputStream extends LispStream {

	/**
	 * Writes the provided {@code aChar} out to the stream.
	 *
	 * @param aChar
	 * 		the character to write out to the stream
	 */
	void writeChar(int aChar);

	/**
	 * Writes the provided {@code aByte} out to the stream.
	 *
	 * @param aByte
	 * 		the byte to write out to the stream
	 */
	void writeByte(int aByte);

	/**
	 * Writes the provided {@code outputString} from the provided {@code start} index to the provided {@code end} index
	 * to the stream.
	 *
	 * @param outputString
	 * 		the string to write out to the stream
	 * @param start
	 * 		the starting index
	 * @param end
	 * 		the ending index
	 */
	void writeString(String outputString, int start, int end);

	/**
	 * Clears the output from the stream.
	 */
	void clearOutput();

	/**
	 * Finishes the output currently in the stream.
	 */
	void finishOutput();

	/**
	 * Forces the output through the stream.
	 */
	void forceOutput();

	@Override
	default boolean isOutputStream() {
		return true;
	}
}
