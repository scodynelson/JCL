package jcl.lang;

import java.io.InputStream;
import java.io.OutputStream;

import jcl.lang.internal.stream.BinaryNativeStreamStructImpl;

/**
 * The {@link BinaryNativeStreamStruct} is the object representation of a binary reading and writing system level Lisp
 * stream.
 */
public interface BinaryNativeStreamStruct extends IOStreamStruct {

	static BinaryNativeStreamStruct toBinaryNativeStream(final InputStream inputStream,
	                                                            final OutputStream outputStream) {
		return new BinaryNativeStreamStructImpl(inputStream, outputStream);
	}
}
