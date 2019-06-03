/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal.stream;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.PushbackReader;
import java.nio.charset.Charset;
import java.util.concurrent.CompletableFuture;

import jcl.lang.BooleanStruct;
import jcl.lang.CharacterStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.TStruct;
import jcl.lang.condition.exception.EndOfFileException;
import jcl.lang.condition.exception.StreamErrorException;
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.stream.ReadCharResult;
import org.apache.commons.lang3.SystemUtils;

/**
 * The {@link CharacterStreamStructImpl} is the object representation of a character reading and writing Lisp stream.
 */
public final class CharacterStreamStructImpl extends StreamStructImpl implements CharacterStreamStruct {

	/**
	 * The maximum size of internal buffer array to allocate in the {@link PushbackReader} {@link #reader}.
	 */
	private static final int PUSHBACK_BUFFER_SIZE = Short.MAX_VALUE;

	/**
	 * The {@link PushbackReader} for reading input.
	 */
	private final PushbackReader reader;

	/**
	 * The {@link PrintWriter} for writing output.
	 */
	private final PrintWriter writer;

	/**
	 * The last character read from the {@link #reader}.
	 */
	private char lastChar;

	/**
	 * Whether or not the end of the stream has been reached.
	 */
	private boolean reachedEndOfStream;

	/**
	 * Public constructor, initializing the {@link #reader} and {@link #writer}.
	 *
	 * @param inputStream
	 * 		the {@link InputStream} to initialize
	 * @param outputStream
	 * 		the {@link OutputStream} to initialize
	 */
	public CharacterStreamStructImpl(final InputStream inputStream, final OutputStream outputStream) {
		super(CommonLispSymbols.CHARACTER);

		final Charset defaultCharset = Charset.defaultCharset();
		reader = new PushbackReader(new InputStreamReader(inputStream, defaultCharset), PUSHBACK_BUFFER_SIZE);
		writer = new PrintWriter(new OutputStreamWriter(outputStream, defaultCharset));
	}

	/*
	CHARACTER-STREAM-STRUCT
	 */

	@Override
	public PushbackReader getJavaReader() {
		return reader;
	}

	@Override
	public PrintWriter getJavaWriter() {
		return writer;
	}

	/*
	INPUT-STREAM-STRUCT
	 */

	@Override
	public ReadCharResult readChar(final boolean eofErrorP, final LispStruct eofValue) {
		try {
			final int readChar = reader.read();

			if (readChar == -1) {
				reachedEndOfStream = true;

				if (eofErrorP) {
					throw new EndOfFileException(this);
				} else {
					return new ReadCharResult(eofValue);
				}
			} else {
				return getReadPeekResult(readChar);
			}
		} catch (final IOException ioe) {
			throw new StreamErrorException("Failed to read next character.", ioe, this);
		}
	}

	private ReadCharResult getReadPeekResult(final int readChar) {
		final ReadCharResult readCharResult = new ReadCharResult(readChar);

		if ((readChar == '\r') && SystemUtils.IS_OS_WINDOWS) {
			final ReadCharResult tempReadCharResult = readChar(false, null);
			if (tempReadCharResult.isEof()) {
				return readCharResult;
			}

			final Integer result = tempReadCharResult.getResult();
			if (result == '\n') {
				lineNumber++;
				return tempReadCharResult;
			} else {
				unreadChar(result);
				return readCharResult;
			}
		}

		if (readChar == '\n') {
			lineNumber++;
		}
		return readCharResult;
	}

	@Override
	public ReadCharResult readCharNoHang(final boolean eofErrorP, final LispStruct eofValue) {
		try {
			if (reader.ready()) {
				return readChar(eofErrorP, eofValue);
			} else {
				return new ReadCharResult(-5);
			}
		} catch (final IOException ioe) {
			throw new StreamErrorException("Failed to read next character.", ioe, this);
		}
	}

	@Override
	public Integer unreadChar(final Integer codePoint) {
		try {
			if (codePoint == '\n') {
				lineNumber--;
			}
			reader.unread(codePoint);

			reachedEndOfStream = false;

			return codePoint;
		} catch (final IOException ioe) {
			throw new StreamErrorException("Failed to unread previous character.", ioe, this);
		}
	}

	@Override
	public LispStruct clearInput() {
		try {
			ReadCharResult result = new ReadCharResult(0);
			while (reader.ready() && !result.isEof()) {
				result = readChar(false, null);
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
			if (reader.ready()) {

				final int n = readChar(false, null).getResult();
				if (n < 0) {
					return NILStruct.INSTANCE;
				}

				unreadChar(n);

				return TStruct.INSTANCE;
			} else {
				return NILStruct.INSTANCE;
			}
		} catch (final IOException ioe) {
			throw new StreamErrorException("Failed to listen on stream.", ioe, this);
		}
	}

	/*
	OUTPUT-STREAM-STRUCT
	 */

	@Override
	public void writeChar(final int codePoint) {
		if ((codePoint == '\n') && SystemUtils.IS_OS_WINDOWS && (lastChar != '\r')) {
			writer.write('\r');
			writer.write('\n');
			writer.flush();
			lastChar = '\n';
		} else {
			writer.write((char) codePoint);
			lastChar = (char) codePoint;
		}
	}

	@Override
	public void writeString(final String outputString) {
		writer.write(outputString);
		if (outputString.contains("\n")) {
			writer.flush();
		}

		lastChar = outputString.charAt(outputString.length() - 1);
	}

	@Override
	public void writeLine(final String outputString) {
		writer.write(outputString);
		writeChar('\n');
	}

	@Override
	public BooleanStruct freshLine() {
		if (lastChar == '\n') {
			return NILStruct.INSTANCE;
		}
		writeChar('\n');
		return TStruct.INSTANCE;
	}

	@Override
	public BooleanStruct terpri() {
		writeChar('\n');
		return NILStruct.INSTANCE;
	}

	@Override
	public LispStruct clearOutput() {
		// Do Nothing. We could force this to happen if we extended the PushbackReader to forcefully clear the
		// buffer, but we haven't implemented that functionality for now.
		return NILStruct.INSTANCE;
	}

	@Override
	public LispStruct finishOutput() {
		writer.flush();
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
			reader.close();
			writer.close();
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not close stream.", ioe, this);
		}
		return super.close(abort);
	}
}
