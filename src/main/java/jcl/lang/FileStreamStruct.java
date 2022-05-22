package jcl.lang;

import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.internal.stream.FileStreamStructImpl;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.SystemUtils;

/**
 * The {@link FileStreamStruct} is the object representation of a Lisp 'file-stream' type.
 */
public interface FileStreamStruct extends IOStreamStruct {

	/**
	 * Returns the {@link PathnameStruct} of the underlying file for the stream.
	 *
	 * @return the {@link PathnameStruct} of the underlying file for the stream
	 */
	PathnameStruct toPathname();

	/**
	 * Returns the external-format containing file format details for reading and writing operations.
	 *
	 * @return the external-format containing file format details for reading and writing operations
	 */
	LispStruct streamExternalFormat();

	/**
	 * Returns the length of the Character or String object that would be read or written to the stream.
	 *
	 * @param object
	 * 		the Character or String object to find the length of for file operations
	 *
	 * @return the length of the Character or String object that would be read or written to the stream
	 */
	static LispStruct fileStringLength(final LispStruct object) {
		if (object instanceof CharacterStruct) {
			final char javaChar = ((CharacterStruct) object).toJavaChar();
			if ((javaChar == '\n') && SystemUtils.IS_OS_WINDOWS) {
				return IntegerStruct.TWO;
			}
			return IntegerStruct.ONE;
		} else if (object instanceof StringStruct) {
			final String javaString = ((StringStruct) object).toJavaString();

			final int length;
			if (SystemUtils.IS_OS_WINDOWS) {
				final int newlineCount = StringUtils.countMatches(javaString, '\n');
				length = javaString.length() + newlineCount;
			} else {
				length = javaString.length();
			}
			return IntegerStruct.toLispInteger(length);
		} else {
			throw new TypeErrorException(object + " must be a Character or a String");
		}
	}

	/**
	 * Returns a new File-Stream instance for reading from and/or writing to files.
	 *
	 * @param pathname
	 * 		the {@link PathnameStruct} indicating the location of the file
	 * @param direction
	 * 		the direction (:input, :output, :io, :probe) to interact with the file
	 * @param elementType
	 * 		the type of the elements in the underlying stream
	 * @param ifExists
	 * 		symbol indicating specialized behavior if the file currently exists
	 * @param ifDoesNotExist
	 * 		symbol indicating specialized behavior if the file does not currently exist
	 * @param externalFormat
	 * 		the external-format containing file format details
	 *
	 * @return a new File-Stream instance
	 */
	static FileStreamStruct toFileStream(final PathnameStruct pathname,
	                                     final SymbolStruct direction,
	                                     final LispStruct elementType,
	                                     final SymbolStruct ifExists,
	                                     final SymbolStruct ifDoesNotExist,
	                                     final LispStruct externalFormat) {
		return new FileStreamStructImpl(pathname, direction, elementType, ifExists, ifDoesNotExist, externalFormat);
	}
}
