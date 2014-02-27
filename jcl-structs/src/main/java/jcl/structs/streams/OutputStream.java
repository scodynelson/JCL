package jcl.structs.streams;

import jcl.structs.conditions.exceptions.StreamErrorException;

/**
 * The {@code OutputStream} is the representation for all Lisp output 'stream' types.
 */
public interface OutputStream extends LispStream {

	/**
	 * This method writes the provided {@code aChar} out to the stream.
	 *
	 * @param aChar the character to write out to the stream
	 * @throws StreamErrorException if the character could not be written to the stream
	 */
	void writeChar(int aChar) throws StreamErrorException;

	/**
	 * This method writes the provided {@code aByte} out to the stream.
	 *
	 * @param aByte the byte to write out to the stream
	 * @throws StreamErrorException if the byte could not be written to the stream
	 */
	void writeByte(int aByte) throws StreamErrorException;

	/**
	 * This method writes the provided {@code outputString} from the provided {@code start} index to the provided {@code end}
	 * index to the stream.
	 *
	 * @param outputString the string to write out to the stream
	 * @param start        the starting index
	 * @param end          the ending index
	 * @throws StreamErrorException if the string could not be written to the stream
	 */
	void writeString(String outputString, int start, int end) throws StreamErrorException;

	/**
	 * This method clears the output from the stream.
	 */
	void clearOutput();

	/**
	 * This method finishes the output currently in the stream.
	 */
	void finishOutput();

	/**
	 * This method forces the output through the stream.
	 */
	void forceOutput();
}
