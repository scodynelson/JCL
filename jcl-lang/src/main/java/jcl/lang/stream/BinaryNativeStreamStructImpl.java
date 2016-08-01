/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.stream;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import jcl.lang.LispStruct;
import jcl.lang.condition.exception.StreamErrorException;
import jcl.type.StreamType;
import jcl.type.UnsignedByteType;

/**
 * The {@link BinaryNativeStreamStructImpl} is the object representation of a binary reading and writing system level Lisp
 * stream.
 */
public final class BinaryNativeStreamStructImpl extends AbstractNativeStreamStructImpl {

	/**
	 * The {@link InputStream} for reading input.
	 */
	private final InputStream inputStream;

	/**
	 * The {@link OutputStream} for writing output.
	 */
	private final OutputStream outputStream;

	/**
	 * Public constructor.
	 *
	 * @param inputStream
	 * 		the {@link InputStream} to create a BinaryStreamStruct from
	 * @param outputStream
	 * 		the {@link OutputStream} to create a BinaryStreamStruct from
	 */
	private BinaryNativeStreamStructImpl(final InputStream inputStream, final OutputStream outputStream) {
		this(false, inputStream, outputStream);
	}

	/**
	 * Public constructor.
	 *
	 * @param interactive
	 * 		whether or not the struct created is 'interactive'
	 * @param inputStream
	 * 		the {@link InputStream} to create a BinaryStreamStruct from
	 * @param outputStream
	 * 		the {@link OutputStream} to create a BinaryStreamStruct from
	 */
	private BinaryNativeStreamStructImpl(final boolean interactive, final InputStream inputStream, final OutputStream outputStream) {
		super(StreamType.INSTANCE, interactive, UnsignedByteType.INSTANCE);

		this.inputStream = new BufferedInputStream(inputStream);
		this.outputStream = new BufferedOutputStream(outputStream);
	}

	public static BinaryNativeStreamStructImpl valueOf(final InputStream inputStream, final OutputStream outputStream) {
		return new BinaryNativeStreamStructImpl(inputStream, outputStream);
	}

	public static BinaryNativeStreamStructImpl valueOf(final boolean interactive, final InputStream inputStream, final OutputStream outputStream) {
		return new BinaryNativeStreamStructImpl(interactive, inputStream, outputStream);
	}

	@Override
	public ReadPeekResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		throw new StreamErrorException(StreamUtils.OPERATION_ONLY_CHARACTER_STREAM, this);
	}

	@Override
	public ReadPeekResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		try {
			final int readByte = inputStream.read();
			return StreamUtils.getReadPeekResult(this, readByte, eofErrorP, eofValue);
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_READ_BYTE, ioe, this);
		}
	}

	@Override
	public ReadPeekResult peekChar(final PeekType peekType, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		throw new StreamErrorException(StreamUtils.OPERATION_ONLY_CHARACTER_STREAM, this);
	}

	@Override
	public Integer unreadChar(final Integer codePoint) {
		throw new StreamErrorException(StreamUtils.OPERATION_ONLY_CHARACTER_STREAM, this);
	}

	@Override
	public void clearInput() {
		try {
			int n = 0;
			while (inputStream.available() > 0) {
				n = inputStream.read();
			}
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not clear input for stream.", ioe, this);
		}
	}

	@Override
	public void writeChar(final int aChar) {
		throw new StreamErrorException(StreamUtils.OPERATION_ONLY_CHARACTER_STREAM, this);
	}

	@Override
	public void writeByte(final int aByte) {
		try {
			outputStream.write(aByte);
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_WRITE_BYTE, ioe, this);
		}
	}

	@Override
	public void writeString(final String outputString, final int start, final int end) {
		throw new StreamErrorException(StreamUtils.OPERATION_ONLY_CHARACTER_STREAM, this);
	}

	@Override
	public void clearOutput() {
		try {
			outputStream.flush();
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not clear output for stream.", ioe, this);
		}
	}

	@Override
	public void finishOutput() {
		try {
			outputStream.flush();
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not finish output for stream.", ioe, this);
		}
	}

	@Override
	public void forceOutput() {
		try {
			outputStream.flush();
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not force output for stream.", ioe, this);
		}
	}

	@Override
	public boolean isStartOfLine() {
		return false;
	}

	@Override
	public boolean close() {
		try {
			inputStream.close();
			outputStream.close();
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not close stream.", ioe, this);
		}
		return super.close();
	}

	@Override
	public Long fileLength() {
		throw new StreamErrorException(StreamUtils.OPERATION_ONLY_FILE_STREAM, this);
	}

	@Override
	public Long filePosition(final Long filePosition) {
		return null;
	}
}
