package jcl.lang;

import java.io.InputStream;

import jcl.lang.internal.stream.StringOutputStreamStructImpl;

/**
 * The {@link StringOutputStreamStruct} is the object representation of a Lisp 'string-stream' output type.
 */
public interface StringOutputStreamStruct extends OutputStreamStruct {

	/**
	 * Returns the output stream as a {@link StringStruct}. This operation also empties the underlying data that exists
	 * in the stream.
	 *
	 * @return the output stream as a {@link StringStruct}
	 */
	StringStruct getOutputStreamString();

	/**
	 * Returns a new String-Output-Stream instance for writing characters to a string.
	 *
	 * @param elementType
	 * 		the element type of the string data
	 *
	 * @return a new String-Output-Stream instance
	 */
	static StringOutputStreamStruct toStringOutputStream(final LispStruct elementType) {
		return new StringOutputStreamStructImpl(elementType);
	}
}
