package jcl.lang;

import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.PushbackReader;

import jcl.lang.condition.exception.StreamErrorException;
import jcl.lang.internal.stream.CharacterStreamStructImpl;
import jcl.lang.stream.ReadCharResult;

/**
 * The {@link CharacterStreamStruct} is the object representation of a character reading and writing Lisp stream.
 */
public interface CharacterStreamStruct extends IOStreamStruct {

	String OPERATION_UNSUPPORTED = "Operation not supported for Character Streams.";

	/*
	CHARACTER-STREAM-STRUCT
	 */

	/**
	 * Returns the underlying Java {@link PushbackReader} for this Character-Stream.
	 *
	 * @return the underlying Java {@link PushbackReader} for this Character Stream
	 */
	PushbackReader getJavaReader();

	/**
	 * Returns the underlying Java {@link PrintWriter} for this Character-Stream.
	 *
	 * @return the underlying Java {@link PrintWriter} for this Character-Stream
	 */
	PrintWriter getJavaWriter();

	/**
	 * Returns a new Character-Stream instance that will read input from the provided {@link InputStream} and write
	 * output to the provided {@link OutputStream}.
	 *
	 * @param inputStream
	 * 		the {@link InputStream} where input will be read from
	 * @param outputStream
	 * 		the {@link OutputStream} where output will be written to
	 *
	 * @return a new Character-Stream instance
	 */
	static CharacterStreamStruct toCharacterStream(final InputStream inputStream, final OutputStream outputStream) {
		return new CharacterStreamStructImpl(inputStream, outputStream);
	}

	/*
	INPUT-STREAM-STRUCT
	 */

	@Override
	default ReadCharResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		throw new StreamErrorException(OPERATION_UNSUPPORTED, this);
	}

	/*
	OUTPUT-STREAM-STRUCT
	 */

	@Override
	default void writeByte(final int aByte) {
		throw new StreamErrorException(OPERATION_UNSUPPORTED, this);
	}
}
