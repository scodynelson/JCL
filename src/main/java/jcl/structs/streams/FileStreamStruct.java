/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.structs.streams;

import jcl.LispStruct;
import jcl.LispType;
import jcl.structs.conditions.exceptions.StreamErrorException;
import jcl.types.FileStream;
import jcl.types.SignedByte;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.EOFException;
import java.io.IOException;
import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.channels.FileChannel;
import java.nio.charset.Charset;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;

/**
 * The {@link FileStreamStruct} is the object representation of a Lisp 'file-stream' type.
 */
public class FileStreamStruct extends AbstractNativeStreamStruct {

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(FileStreamStruct.class);

	/**
	 * The {@link FileChannel} to read from and write to.
	 */
	private final FileChannel fileChannel;

	/**
	 * The buffer size for the {@link #fileChannel}.
	 */
	private final int bufferSize;

	/**
	 * Public constructor.
	 *
	 * @param path
	 * 		the file to create a FileStreamStruct from
	 */
	public FileStreamStruct(final Path path) {
		this(false, path);
	}

	/**
	 * Public constructor.
	 *
	 * @param interactive
	 * 		whether or not the struct created is 'interactive'
	 * @param path
	 * 		the {@link Path} to create a FileStreamStruct from
	 */
	public FileStreamStruct(final boolean interactive, final Path path) {
		super(FileStream.INSTANCE, interactive, getElementType2(path));

		try {
			fileChannel = FileChannel.open(path, StandardOpenOption.READ, StandardOpenOption.WRITE);
			bufferSize = BigInteger.valueOf(fileChannel.size()).intValueExact();
		} catch (final IOException ioe) {
			throw new StreamErrorException("Failed to open provided file.", ioe);
		}
	}

	/**
	 * Used to retrieve the element type for object construction.
	 *
	 * @param path
	 * 		the {@link Path}s to create a FileStreamStruct from
	 *
	 * @return the element type for object construction
	 */
	private static LispType getElementType2(final Path path) {
		try (FileChannel fileChannel = FileChannel.open(path, StandardOpenOption.READ)) {
			final long bufferSize = fileChannel.size();
			final BigInteger bits = BigInteger.valueOf(bufferSize);
			return SignedByte.Factory.getInstance(bits);
		} catch (final IOException ioe) {
			throw new StreamErrorException("Failed to open provided file.", ioe);
		}
	}

	@Override
	public ReadPeekResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		final int readChar = readChar(StreamUtils.FAILED_TO_READ_CHAR);
		return StreamUtils.getReadPeekResult(readChar, eofErrorP, eofValue);
	}

	/**
	 * Internal read method for reading a character from the fileStream.
	 *
	 * @param streamErrorString
	 * 		the error string to use when an {@link IOException} occurs
	 *
	 * @return the next character in the fileStream
	 */
	private int readChar(final String streamErrorString) {
		try {
			final ByteBuffer byteBuffer = ByteBuffer.allocate(bufferSize);
			fileChannel.read(byteBuffer);

			final CharBuffer charBuffer = Charset.defaultCharset().decode(byteBuffer);
			return charBuffer.get();
		} catch (final EOFException eofe) {
			if (LOGGER.isWarnEnabled()) {
				LOGGER.warn(StreamUtils.END_OF_FILE_REACHED, eofe);
			}
			return -1;
		} catch (final IOException ioe) {
			throw new StreamErrorException(streamErrorString, ioe);
		}
	}

	@Override
	public ReadPeekResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		final int readByte = readByte();
		return StreamUtils.getReadPeekResult(readByte, eofErrorP, eofValue);
	}

	/**
	 * Internal read method for reading a byte from the fileStream.
	 *
	 * @return the next byte in the fileStream
	 */
	private int readByte() {
		try {
			final ByteBuffer byteBuffer = ByteBuffer.allocate(bufferSize);
			return fileChannel.read(byteBuffer);
		} catch (final EOFException eofe) {
			if (LOGGER.isWarnEnabled()) {
				LOGGER.warn(StreamUtils.END_OF_FILE_REACHED, eofe);
			}
			return -1;
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_READ_BYTE, ioe);
		}
	}

	@Override
	public ReadPeekResult peekChar(final PeekType peekType, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {

		final int nextChar;
		switch (peekType.getType()) {
			case NIL:
				nextChar = nilPeekCharFSS();
				break;
			case T:
				nextChar = tPeekCharFSS();
				break;
			case CHARACTER:
				nextChar = characterPeekCharFSS(peekType.getCodePoint());
				break;
			default:
				nextChar = -1;
				break;
		}

		return StreamUtils.getReadPeekResult(nextChar, eofErrorP, eofValue);
	}

	/**
	 * Attempts to peek ahead to the next available character in the stream.
	 *
	 * @return the character peeked from the stream
	 */
	private int nilPeekCharFSS() {
		try {
			final int nextChar = readChar(StreamUtils.FAILED_TO_PEEK_CHAR);
			fileChannel.position(fileChannel.position() - 1);
			return nextChar;
		} catch (final EOFException eofe) {
			if (LOGGER.isWarnEnabled()) {
				LOGGER.warn(StreamUtils.END_OF_FILE_REACHED, eofe);
			}
			return -1;
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_PEEK_CHAR, ioe);
		}
	}

	/**
	 * Attempts to peek ahead to the next available non-whitespace character in the stream.
	 *
	 * @return the character peeked from the stream
	 */
	private int tPeekCharFSS() {
		try {
			// Initialize to whitespace, since we are attempting to skip it anyways
			int nextChar = ' ';

			int i = 0;
			while (Character.isWhitespace(nextChar) && (nextChar != -1)) {
				nextChar = readChar(StreamUtils.FAILED_TO_PEEK_CHAR);
				i++;
			}

			if (nextChar != -1) {
				fileChannel.position(fileChannel.position() - i);
			}
			return nextChar;
		} catch (final EOFException eofe) {
			if (LOGGER.isWarnEnabled()) {
				LOGGER.warn(StreamUtils.END_OF_FILE_REACHED, eofe);
			}
			return -1;
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_PEEK_CHAR, ioe);
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
	private int characterPeekCharFSS(final Integer codePoint) {
		try {
			// Initialize to 0 value
			int nextChar = 0;

			int i = 0;
			while ((nextChar != codePoint) && (nextChar != -1)) {
				nextChar = readChar(StreamUtils.FAILED_TO_PEEK_CHAR);
				i++;
			}

			if (nextChar != -1) {
				fileChannel.position(fileChannel.position() - i);
			}
			return nextChar;
		} catch (final EOFException eofe) {
			if (LOGGER.isWarnEnabled()) {
				LOGGER.warn(StreamUtils.END_OF_FILE_REACHED, eofe);
			}
			return -1;
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_PEEK_CHAR, ioe);
		}
	}

	@Override
	public Integer unreadChar(final Integer codePoint) {
		try {
			fileChannel.position(fileChannel.position() - 1);
			return codePoint;
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_UNREAD_CHAR, ioe);
		}
	}

	@Override
	public void clearInput() {
		// Do nothing.
	}

	@Override
	public void writeChar(final int aChar) {
		try {
			final char[] chars = {(char) aChar};
			final CharBuffer charBuffer = CharBuffer.wrap(chars);

			final ByteBuffer byteBuffer = Charset.defaultCharset().encode(charBuffer);
			fileChannel.write(byteBuffer);
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_WRITE_CHAR, ioe);
		}
	}

	@Override
	public void writeByte(final int aByte) {
		try {
			final ByteBuffer byteBuffer = ByteBuffer.allocate(aByte);
			fileChannel.write(byteBuffer);
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_WRITE_BYTE, ioe);
		}
	}

	@Override
	public void writeString(final String outputString, final int start, final int end) {
		try {
			final String subString = outputString.substring(start, end);
			final CharBuffer charBuffer = CharBuffer.wrap(subString);

			final ByteBuffer byteBuffer = Charset.defaultCharset().encode(charBuffer);
			fileChannel.write(byteBuffer);
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_WRITE_STRING, ioe);
		}
	}

	@Override
	public void clearOutput() {
		// Do nothing.
	}

	@Override
	public void finishOutput() {
		try {
			fileChannel.force(true);
		} catch (final IOException ioe) {
			if (LOGGER.isWarnEnabled()) {
				LOGGER.warn("Could not finish stream output.", ioe);
			}
		}
	}

	@Override
	public void forceOutput() {
		try {
			fileChannel.force(false);
		} catch (final IOException ioe) {
			if (LOGGER.isWarnEnabled()) {
				LOGGER.warn("Could not force stream output.", ioe);
			}
		}
	}

	@Override
	public void close() {
		try {
			fileChannel.close();
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not close stream.", ioe);
		}
		super.close();
	}

	@Override
	public Long fileLength() {
		try {
			return fileChannel.size();
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not retrieve file length.", ioe);
		}
	}

	@Override
	public Long filePosition(final Long filePosition) {
		try {
			if (filePosition != null) {
				fileChannel.position(filePosition);
			}
			return fileChannel.position();
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not retrieve file position.", ioe);
		}
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
