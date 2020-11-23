package jcl.lang;

import java.io.InputStream;
import java.io.OutputStream;

import jcl.lang.condition.exception.StreamErrorException;
import jcl.lang.internal.stream.BinaryStreamStructImpl;

/**
 * The {@link BinaryStreamStruct} is the object representation of a binary reading and writing Lisp stream.
 */
public interface BinaryStreamStruct extends IOStreamStruct {

	String OPERATION_UNSUPPORTED = "Operation not supported for Binary Streams.";

	/**
	 * Returns the underlying Java {@link InputStream} for this Binary-Stream.
	 *
	 * @return the underlying Java {@link InputStream} for this Binary-Stream
	 */
	InputStream getJavaInputStream();

	/**
	 * Returns the underlying Java {@link OutputStream} for this Binary-Stream.
	 *
	 * @return the underlying Java {@link OutputStream} for this Binary-Stream
	 */
	OutputStream getJavaOutputStream();

	/**
	 * Returns a new Binary-Stream instance that will read input from the provided {@link InputStream} and write output
	 * to the provided {@link OutputStream}.
	 *
	 * @param inputStream
	 * 		the {@link InputStream} where input will be read from
	 * @param outputStream
	 * 		the {@link OutputStream} where output will be written to
	 *
	 * @return a new Binary-Stream instance
	 */
	static BinaryStreamStruct toCharacterStream(final InputStream inputStream, final OutputStream outputStream) {
		return new BinaryStreamStructImpl(inputStream, outputStream);
	}

	/*
	INPUT-STREAM-STRUCT
	 */

	@Override
	default ReadCharResult readChar(final boolean eofErrorP, final LispStruct eofValue) {
		throw new StreamErrorException(OPERATION_UNSUPPORTED, this);
	}

	@Override
	default ReadCharResult readCharNoHang(final boolean eofErrorP, final LispStruct eofValue) {
		throw new StreamErrorException(OPERATION_UNSUPPORTED, this);
	}

	@Override
	default Integer unreadChar(final Integer codePoint) {
		throw new StreamErrorException(OPERATION_UNSUPPORTED, this);
	}

	/*
	OUTPUT-STREAM-STRUCT
	 */

	@Override
	default void writeChar(final int codePoint) {
		throw new StreamErrorException(OPERATION_UNSUPPORTED, this);
	}

	@Override
	default void writeString(final String outputString) {
		throw new StreamErrorException(OPERATION_UNSUPPORTED, this);
	}

	@Override
	default void writeLine(final String outputString) {
		throw new StreamErrorException(OPERATION_UNSUPPORTED, this);
	}

	@Override
	default BooleanStruct freshLine() {
		throw new StreamErrorException(OPERATION_UNSUPPORTED, this);
	}

	@Override
	default BooleanStruct terpri() {
		throw new StreamErrorException(OPERATION_UNSUPPORTED, this);
	}
}
