package jcl.lang;

import java.io.InputStream;
import java.io.OutputStream;

import jcl.lang.internal.stream.CharacterNativeStreamStructImpl;

/**
 * The {@link CharacterNativeStreamStruct} is the object representation of a character reading and writing system level Lisp
 * stream.
 */
public interface CharacterNativeStreamStruct extends IOStreamStruct {

	static CharacterNativeStreamStruct toCharacterNativeStream(final InputStream inputStream,
	                                                           final OutputStream outputStream) {
		return new CharacterNativeStreamStructImpl(inputStream, outputStream);
	}
}
