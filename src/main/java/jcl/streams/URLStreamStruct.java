/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;

import jcl.LispStruct;
import jcl.conditions.exceptions.ErrorException;
import jcl.conditions.exceptions.StreamErrorException;
import jcl.types.CharacterType;
import jcl.types.FileStreamType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The {@link URLStreamStruct} is the object representation of a Lisp 'url-stream' type.
 */
public class URLStreamStruct extends AbstractNativeStreamStruct {

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(URLStreamStruct.class);

	/**
	 * The {@link URL} of the http resource that the {@link #urlConnection} interacts with.
	 */
	private final URL url;

	/**
	 * The {@link BufferedReader} for reading input.
	 */
	private final BufferedReader inputStream;

	/**
	 * The {@link URLConnection} to read from and write to.
	 */
	private final URLConnection urlConnection;

	/**
	 * Public constructor.
	 *
	 * @param url
	 * 		the {@link URL} to create a URLStreamStruct from
	 */
	public URLStreamStruct(final URL url) {
		this(false, url);
	}

	/**
	 * Public constructor.
	 *
	 * @param interactive
	 * 		whether or not the struct created is 'interactive'
	 * @param url
	 * 		the {@link URL} to create a URLStreamStruct from
	 */
	public URLStreamStruct(final boolean interactive, final URL url) {
		// TODO: Character Type Stream???
		super(FileStreamType.INSTANCE, interactive, CharacterType.INSTANCE);

		this.url = url;
		try {
			urlConnection = url.openConnection();
			inputStream = new BufferedReader(new InputStreamReader(url.openStream()));
		} catch (final IOException ioe) {
			throw new ErrorException("Failed to open provided url.", ioe);
		}
	}

	/**
	 * Getter for the {@link #url} value.
	 *
	 * @return the {@link #url} value
	 */
	public URL getUrl() {
		return url;
	}

	@Override
	public ReadPeekResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		try {
			inputStream.mark(1);
			final int readChar = inputStream.read();
			return StreamUtils.getReadPeekResult(this, readChar, eofErrorP, eofValue);
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_READ_CHAR, ioe, this);
		}
	}

	@Override
	public ReadPeekResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		throw new StreamErrorException(StreamUtils.OPERATION_ONLY_BINARY_STREAM, this);
	}

	@Override
	public ReadPeekResult peekChar(final PeekType peekType, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {

		final int nextChar;
		switch (peekType.getType()) {
			case NIL:
				nextChar = nilPeekCharUSS();
				break;
			case T:
				nextChar = tPeekCharUSS();
				break;
			case CHARACTER:
				nextChar = characterPeekCharUSS(peekType.getCodePoint());
				break;
			default:
				nextChar = -1;
				break;
		}

		return StreamUtils.getReadPeekResult(this, nextChar, eofErrorP, eofValue);
	}

	/**
	 * Attempts to peek ahead to the next available character in the stream.
	 *
	 * @return the character peeked from the stream
	 */
	private int nilPeekCharUSS() {
		try {
			inputStream.mark(1);

			final int nextChar = inputStream.read();
			inputStream.reset();
			return nextChar;
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_PEEK_CHAR, ioe, this);
		}
	}

	/**
	 * Attempts to peek ahead to the next available non-whitespace character in the stream.
	 *
	 * @return the character peeked from the stream
	 */
	private int tPeekCharUSS() {
		try {
			inputStream.mark(1);

			// Initialize to whitespace, since we are attempting to skip it anyways
			int nextChar = ' ';
			while (Character.isWhitespace(nextChar)) {
				nextChar = inputStream.read();
			}
			inputStream.reset();
			return nextChar;
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_PEEK_CHAR, ioe, this);
		}
	}

	/**
	 * Attempts to peek ahead to the provided {@code codePoint} in the stream.
	 *
	 * @param codePoint
	 * 		the codePoint to peek up to in the stream
	 *
	 * @return the character peeked from the stream
	 */
	private int characterPeekCharUSS(final Integer codePoint) {
		try {
			inputStream.mark(1);

			// Initialize to -1 value, since this is essentially EOF
			int nextChar = -1;
			while (nextChar != codePoint) {
				nextChar = inputStream.read();
			}
			inputStream.reset();
			return nextChar;
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_PEEK_CHAR, ioe, this);
		}
	}

	@Override
	public Integer unreadChar(final Integer codePoint) {
		try {
			inputStream.reset();
			return codePoint;
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_UNREAD_CHAR, ioe, this);
		}
	}

	@Override
	public void clearInput() {
		try {
			inputStream.mark(0);
			inputStream.reset();
		} catch (final IOException ioe) {
			if (LOGGER.isWarnEnabled()) {
				LOGGER.warn("IO exception occurred.", ioe);
			}
		}
	}

	@Override
	public void writeChar(final int aChar) {
//		try {
		// TODO
//		} catch (final IOException ioe) {
//			throw new StreamErrorException(StreamUtils.FAILED_TO_WRITE_CHAR, ioe);
//		}
	}

	@Override
	public void writeByte(final int aByte) {
		throw new StreamErrorException(StreamUtils.OPERATION_ONLY_BINARY_STREAM, this);
	}

	@Override
	public void writeString(final String outputString, final int start, final int end) {
//		try {
		final String subString = outputString.substring(start, end);
		// TODO
//		} catch (final IOException ioe) {
//			throw new StreamErrorException(StreamUtils.FAILED_TO_WRITE_STRING, ioe);
//		}
	}

	@Override
	public void clearOutput() {
		// TODO
	}

	@Override
	public void finishOutput() {
		// TODO
	}

	@Override
	public void forceOutput() {
		// TODO
	}

	@Override
	public boolean close() {
		try {
			inputStream.close();
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
