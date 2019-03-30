package jcl.lang;

import jcl.lang.internal.stream.EchoStreamStructImpl;

/**
 * The {@link EchoStreamStruct} is the object representation of a Lisp 'echo-stream' type.
 */
public interface EchoStreamStruct extends DualStreamStruct {

	default InputStreamStruct echoStreamInputStream() {
		return getInputStreamStruct();
	}

	default OutputStreamStruct echoStreamOutputStream() {
		return getOutputStreamStruct();
	}

	static EchoStreamStruct toEchoStream(final InputStreamStruct inputStreamStruct,
	                                     final OutputStreamStruct outputStreamStruct) {
		return new EchoStreamStructImpl(inputStreamStruct, outputStreamStruct);
	}
}
