/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams;

import jcl.LispStruct;
import jcl.conditions.exceptions.EndOfFileException;

/**
 * A Utility class for stream structures.
 */
final class StreamUtils {

	// TODO: The following should probably go into a properties file...
	static final String END_OF_FILE_REACHED = "End of file reached.";

	static final String FAILED_TO_READ_BYTE = "Failed to read next byte.";

	static final String FAILED_TO_READ_CHAR = "Failed to read next character.";

	static final String FAILED_TO_PEEK_CHAR = "Failed to peek at next character.";

	static final String FAILED_TO_UNREAD_CHAR = "Failed to unread previous character.";

	static final String FAILED_TO_WRITE_BYTE = "Failed to write byte.";

	static final String FAILED_TO_WRITE_CHAR = "Failed to write character.";

	static final String FAILED_TO_WRITE_STRING = "Failed to write string.";

	static final String OPERATION_ONLY_FILE_STREAM = "Operation only supported on a FileStream.";

	static final String OPERATION_ONLY_CHARACTER_STREAM = "Operation only supported for CharacterStreams.";

	static final String OPERATION_ONLY_BINARY_STREAM = "Operation only supported for BinaryStreams.";

	static final String OPERATION_NOT_SUPPORTED_FOR_EMPTY_STREAM = "Operation not supported for EmptyStream.";

	/**
	 * Private constructor.
	 */
	private StreamUtils() {
	}

	/**
	 * Used to get a resulting {@link ReadPeekResult} object from the provided {@code readVal}, throw an error if
	 * {@code eofErrorP} is true and the value is the End-of-File value, or a result with the {@code eofValue} if
	 * {@code eofErrorP} is false.
	 *
	 * @param stream
	 * 		the stream performing the read/peek operation
	 * @param readPeekVal
	 * 		the character or byte read or peeked
	 * @param eofErrorP
	 * 		where or not throw an error if the {@code readPeekVal} is the End-of-File value
	 * @param eofValue
	 * 		the value to return if the {@code readPeekVal} is the End-of-File value
	 *
	 * @return the resulting {@link ReadPeekResult} object
	 */
	static ReadPeekResult getReadPeekResult(final LispStream stream, final int readPeekVal, final boolean eofErrorP,
	                                        final LispStruct eofValue) {
		if (readPeekVal == -1) {
			if (eofErrorP) {
				throw new EndOfFileException(END_OF_FILE_REACHED, stream);
			} else {
				return new ReadPeekResult(eofValue);
			}
		} else {
			return new ReadPeekResult(readPeekVal);
		}
	}
}
