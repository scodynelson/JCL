package jcl.lang;

import jcl.lang.internal.stream.StringInputStreamStructImpl;

/**
 * The {@link StringInputStreamStruct} is the object representation of a Lisp 'string-stream' input type.
 */
public interface StringInputStreamStruct extends InputStreamStruct {

	static StringInputStreamStruct toStringInputStream(final StringStruct inputString) {
		return new StringInputStreamStructImpl(inputString.toJavaString());
	}

	static StringInputStreamStruct toStringInputStream(final StringStruct inputString,
	                                                   final IntegerStruct current,
	                                                   final IntegerStruct end) {
		return new StringInputStreamStructImpl(inputString.toJavaString(), current.toJavaInt(), end.toJavaInt());
	}
}
