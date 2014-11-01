package jcl.structs.streams;

import jcl.LispStruct;
import jcl.reader.syntax.reader.PeekResult;
import jcl.reader.syntax.reader.ReadResult;
import jcl.structs.conditions.exceptions.EndOfFileException;

/**
 * A Utility class for stream structures.
 */
final class StreamUtils {

	static final String END_OF_FILE_REACHED = "End of file reached.";
	static final String FAILED_TO_READ_BYTE = "Failed to read next byte.";
	static final String FAILED_TO_READ_CHAR = "Failed to read next character.";
	static final String FAILED_TO_PEEK_CHAR = "Failed to peek at next character.";
	static final String FAILED_TO_UNREAD_CHAR = "Failed to unread previous character.";
	static final String FAILED_TO_WRITE_BYTE = "Failed to write byte.";
	static final String FAILED_TO_WRITE_CHAR = "Failed to write character.";
	static final String FAILED_TO_WRITE_STRING = "Failed to write string.";
	static final String OPERATION_ONLY_FILESTREAM = "Operation only supported on a FileStream.";
	static final String OPERATION_ONLY_BINARYSTREAM = "Operation only supported for BinaryStreams.";

	/**
	 * Private constructor.
	 */
	private StreamUtils() {
	}

	/**
	 * This method is used to get a resulting read object from the provided {@code readVal}, throw an error if {@code
	 * eofErrorP} is true and the value is the EOF value, or a result with the {@code eofValue} if {@code eofErrorP} is
	 * false.
	 *
	 * @param readVal
	 * 		the character or byte read
	 * @param eofErrorP
	 * 		where or not throw an error if the {@code readVal} is the EOF value
	 * @param eofValue
	 * 		the value to return if the {@code readVal} is the EOF value
	 *
	 * @return the resulting read object
	 */
	static ReadResult getReadResult(final int readVal, final boolean eofErrorP, final LispStruct eofValue) {

		if (readVal == -1) {
			if (eofErrorP) {
				throw new EndOfFileException(END_OF_FILE_REACHED);
			} else {
				return new ReadResult(eofValue);
			}
		} else {
			return new ReadResult(readVal);
		}
	}

	/**
	 * This method is used to get a resulting peek object from the provided {@code nextChar}, throw an error if {@code
	 * eofErrorP} is true and the value is the EOF value, or a result with the {@code eofValue} if {@code eofErrorP} is
	 * false.
	 *
	 * @param nextChar
	 * 		the next character peeked
	 * @param eofErrorP
	 * 		where or not throw an error if the {@code nextChar} is the EOF value
	 * @param eofValue
	 * 		the value to return if the {@code nextChar} is the EOF value
	 *
	 * @return the resulting peek object
	 */
	static PeekResult getPeekResult(final int nextChar, final boolean eofErrorP, final LispStruct eofValue) {
		if (nextChar == -1) {
			if (eofErrorP) {
				throw new EndOfFileException(END_OF_FILE_REACHED);
			} else {
				return new PeekResult(eofValue);
			}
		} else {
			return new PeekResult(nextChar);
		}
	}
}
