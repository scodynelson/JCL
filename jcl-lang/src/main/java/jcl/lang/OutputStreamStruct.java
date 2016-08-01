/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang;

/**
 * The {@link OutputStreamStruct} is the representation for all Lisp output 'stream' types.
 */
public interface OutputStreamStruct extends StreamStruct {

	@Override
	default boolean close(final boolean abort) {
		if (abort) {
			clearOutput();
		} else {
			forceOutput();
		}
		return close();
	}

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
	 */
	default void writeString(final String outputString, final int start) {
		writeString(outputString, start, outputString.length() - 1);
	}

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
	default void writeString(final String outputString, final int start, final int end) {
		final String subString = outputString.substring(start, end);
		subString.chars().forEach(this::writeChar);
	}

	default void writeLine(final String outputString, final int start) {
		writeString(outputString, start);
		writeChar('\n');
	}

	default void writeLine(final String outputString, final int start, final int end) {
		writeString(outputString, start, end);
		writeChar('\n');
	}

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

	boolean isStartOfLine();
}