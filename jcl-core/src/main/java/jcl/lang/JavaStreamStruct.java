package jcl.lang;

import java.io.InputStream;
import java.io.OutputStream;

import jcl.lang.internal.stream.JavaStreamStructImpl;

/**
 * The {@link JavaStreamStruct} is the object representation of a character reading and writing system level Lisp
 * stream.
 */
public interface JavaStreamStruct extends IOStreamStruct {

	static JavaStreamStruct toJavaStream(final InputStream inputStream, final OutputStream outputStream) {
		return new JavaStreamStructImpl(inputStream, outputStream);
	}
}
