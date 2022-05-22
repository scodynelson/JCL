package jcl.lang;

import jcl.lang.internal.stream.StringInputStreamStructImpl;

/**
 * The {@link StringInputStreamStruct} is the object representation of a Lisp 'string-stream' input type.
 */
public interface StringInputStreamStruct extends InputStreamStruct {

	/**
	 * Returns a new String-Input-Stream instance for reading characters from the provided {@link StringStruct}. The
	 * {@code current} and {@code end} values indicate the starting and ending positions in the string that can be read
	 * from.
	 *
	 * @param inputString
	 * 		the {@link StringStruct} to read data from
	 * @param current
	 * 		the starting position in the string that can be read from
	 * @param end
	 * 		the ending position in the string that can be read from
	 *
	 * @return a new String-Input-Stream instance
	 */
	static StringInputStreamStruct toStringInputStream(
			final StringStruct inputString, final FixnumStruct current, final FixnumStruct end
	) {
		return new StringInputStreamStructImpl(inputString.toJavaString(), current.toJavaInt(), end.toJavaInt());
	}
}
