package jcl.streams;

/**
 * The {@code OutputStream} is the representation for all Lisp output 'stream' types.
 */
public interface OutputStream extends LispStream {

	/**
	 * This method writes the provided {@code aChar} out to the stream.
	 *
	 * @param aChar the character to write out to the stream
	 */
	void writeChar(int aChar);

	/**
	 * This method writes the provided {@code aByte} out to the stream.
	 *
	 * @param aByte the byte to write out to the stream
	 */
	void writeByte(int aByte);

	/**
	 * This method writes the provided {@code outputString} from the provided {@code start} index to the provided {@code end}
	 * index to the stream.
	 *
	 * @param outputString the string to write out to the stream
	 * @param start        the starting index
	 * @param end          the ending index
	 */
	void writeString(String outputString, int start, int end);

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
