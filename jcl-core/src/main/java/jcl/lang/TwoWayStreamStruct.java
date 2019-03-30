package jcl.lang;

import jcl.lang.internal.stream.TwoWayStreamStructImpl;

/**
 * The {@link TwoWayStreamStruct} is the object representation of a Lisp 'two-way-stream' type.
 */
public interface TwoWayStreamStruct extends DualStreamStruct {

	default InputStreamStruct twoWayStreamInputStream() {
		return getInputStreamStruct();
	}

	default OutputStreamStruct twoWayStreamOutputStream() {
		return getOutputStreamStruct();
	}

	static TwoWayStreamStruct toTwoWayStream(final InputStreamStruct inputStreamStruct,
	                                         final OutputStreamStruct outputStreamStruct) {
		return new TwoWayStreamStructImpl(inputStreamStruct, outputStreamStruct);
	}

	// TODO: Should set interactive this way??
	static TwoWayStreamStruct toTwoWayStream(final boolean interactive,
	                                         final InputStreamStruct inputStreamStruct,
	                                         final OutputStreamStruct outputStreamStruct) {
		return new TwoWayStreamStructImpl(interactive, inputStreamStruct, outputStreamStruct);
	}
}
