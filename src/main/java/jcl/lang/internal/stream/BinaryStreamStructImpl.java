/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal.stream;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.concurrent.CompletableFuture;

import jcl.lang.BinaryStreamStruct;
import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.ReadCharResult;
import jcl.lang.TStruct;
import jcl.lang.condition.exception.EndOfFileException;
import jcl.lang.condition.exception.StreamErrorException;
import jcl.lang.statics.CommonLispSymbols;

/**
 * The {@link BinaryStreamStructImpl} is the object representation of a binary reading and writing Lisp stream.
 */
public final class BinaryStreamStructImpl extends StreamStructImpl implements BinaryStreamStruct {

	/**
	 * The {@link BufferedInputStream} for reading input.
	 */
	private final BufferedInputStream inputStream;

	/**
	 * The {@link BufferedOutputStream} for writing output.
	 */
	private final BufferedOutputStream outputStream;

	/**
	 * Whether or not the end of the stream has been reached.
	 */
	private boolean reachedEndOfStream;

	/**
	 * Public constructor, initializing the {@link #inputStream} and {@link #outputStream}.
	 *
	 * @param inputStream
	 * 		the {@link InputStream} to initialize
	 * @param outputStream
	 * 		the {@link OutputStream} to initialize
	 */
	public BinaryStreamStructImpl(final InputStream inputStream, final OutputStream outputStream) {
		super(CommonLispSymbols.UNSIGNED_BYTE);

		this.inputStream = new BufferedInputStream(inputStream);
		this.outputStream = new BufferedOutputStream(outputStream);
	}

	/*
	BINARY-STREAM-STRUCT
	 */

	@Override
	public InputStream getJavaInputStream() {
		return inputStream;
	}

	@Override
	public OutputStream getJavaOutputStream() {
		return outputStream;
	}

	/*
	INPUT-STREAM-STRUCT
	 */

	@Override
	public ReadCharResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		try {
			final int readByte = inputStream.read();

			if (readByte == -1) {
				reachedEndOfStream = true;

				if (eofErrorP) {
					throw new EndOfFileException(this);
				} else {
					return new ReadCharResult(eofValue);
				}
			} else {
				return new ReadCharResult(readByte);
			}
		} catch (final IOException ioe) {
			throw new StreamErrorException("Failed to read next byte.", ioe, this);
		}
	}

	@Override
	public LispStruct clearInput() {
		try {
			while (inputStream.available() > 0) {
				final int n = inputStream.read();

				if (n == -1) {
					reachedEndOfStream = true;
				}
			}
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not clear input for stream.", ioe, this);
		}
		return NILStruct.INSTANCE;
	}

	@Override
	public BooleanStruct listen() {
		if (reachedEndOfStream) {
			return NILStruct.INSTANCE;
		}

		try {
			return (inputStream.available() == 0) ? TStruct.INSTANCE : NILStruct.INSTANCE;
		} catch (final IOException ioe) {
			throw new StreamErrorException("Failed to listen on stream.", ioe, this);
		}
	}

	/*
	OUTPUT-STREAM-STRUCT
	 */

	@Override
	public void writeByte(final int aByte) {
		if ((aByte < 0) || (aByte > 255)) {
			throw new StreamErrorException("Value provided is not a proper 8-bit byte: " + aByte, this);
		}
		try {
			outputStream.write(aByte);
		} catch (final IOException ioe) {
			throw new StreamErrorException("Failed to write byte.", ioe, this);
		}
	}

	@Override
	public LispStruct clearOutput() {
		// Do Nothing. We could force this to happen if we extended the BufferedOutputStream to forcefully clear the
		// buffer, but we haven't implemented that functionality for now.
		return NILStruct.INSTANCE;
	}

	@Override
	public LispStruct finishOutput() {
		try {
			outputStream.flush();
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not finish output for stream.", ioe, this);
		}
		return NILStruct.INSTANCE;
	}

	@Override
	public LispStruct forceOutput() {
		CompletableFuture.runAsync(this::finishOutput);
		return NILStruct.INSTANCE;
	}

	/*
	STREAM-STRUCT
	 */

	@Override
	public BooleanStruct close(final BooleanStruct abort) {
		if (abort.toJavaPBoolean()) {
			clearOutput();
		} else {
			forceOutput();
		}
		try {
			inputStream.close();
			outputStream.close();
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not close stream.", ioe, this);
		}
		return super.close(abort);
	}
}
