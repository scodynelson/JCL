package jcl.lang;

import jcl.lang.internal.stream.StringOutputStreamStructImpl;
import jcl.type.LispType;

/**
 * The {@link StringOutputStreamStruct} is the object representation of a Lisp 'string-stream' output type.
 */
public interface StringOutputStreamStruct extends OutputStreamStruct {

	String getStreamString();

	void clearStream();

	default StringStruct getOutputStreamString() {
		final String streamString = getStreamString();
		clearStream();
		return StringStruct.toLispString(streamString);
	}

	static StringOutputStreamStruct toStringOutputStream() {
		return new StringOutputStreamStructImpl();
	}

	static StringOutputStreamStruct toStringOutputStream(final LispType elementType) {
		return new StringOutputStreamStructImpl(elementType);
	}
}
