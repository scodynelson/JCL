/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.stream;

import java.io.EOFException;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.math.BigInteger;
import java.nio.channels.FileChannel;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.function.Supplier;

import jcl.lang.LispStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.StreamErrorException;
import jcl.lang.pathname.PathnameStruct;
import jcl.type.FileStreamType;
import jcl.type.LispType;
import jcl.type.SignedByteType;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * The {@link FileStreamStruct} is the object representation of a Lisp 'file-stream' type.
 */
public final class FileStreamStruct extends AbstractNativeStreamStruct {

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(FileStreamStruct.class);

	/**
	 * The {@link Path} of the file that the {@link #randomAccessFile} interacts with.
	 */
	private final Path path;

	/**
	 * The {@link RandomAccessFile} used for reading and writing file information.
	 */
	private final RandomAccessFile randomAccessFile;

	/**
	 * Public constructor.
	 *
	 * @param path
	 * 		the file to create a FileStreamStruct from
	 */
	private FileStreamStruct(final Path path) {
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
	private FileStreamStruct(final boolean interactive, final Path path) {
		super(FileStreamType.INSTANCE, interactive, getElementType2(path));

		this.path = path;
		try {
			randomAccessFile = new RandomAccessFile(path.toFile(), "rw");
		} catch (final FileNotFoundException fnfe) {
			throw new ErrorException("Failed to open provided file.", fnfe);
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
			return SignedByteType.Factory.getInstance(bits);
		} catch (final IOException ioe) {
			throw new ErrorException("Failed to open provided file.", ioe);
		}
	}

	public static FileStreamStruct valueOf(final Path path) {
		return new FileStreamStruct(path);
	}

	public static FileStreamStruct valueOf(final boolean interactive, final Path path) {
		return new FileStreamStruct(interactive, path);
	}

	/**
	 * Getter for the {@link #path} value.
	 *
	 * @return the {@link #path} value
	 */
	public Path getPath() {
		return path;
	}

	@Override
	public Supplier<PathnameStruct> asPathname() {
		return () -> {
			final File file = path.toFile();
			final String namestring = file.getAbsolutePath();
			return PathnameStruct.valueOf(namestring);
		};
	}

	@Override
	public ReadPeekResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		final int readChar = readChar(StreamUtils.FAILED_TO_READ_CHAR);
		return StreamUtils.getReadPeekResult(this, readChar, eofErrorP, eofValue);
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
			return randomAccessFile.readUnsignedByte();
		} catch (final EOFException eofe) {
			if (LOGGER.isTraceEnabled()) {
				LOGGER.trace(StreamUtils.END_OF_FILE_REACHED, eofe);
			}
			return -1;
		} catch (final IOException ioe) {
			throw new StreamErrorException(streamErrorString, ioe, this);
		}
	}

	@Override
	public ReadPeekResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		final int readByte = readByte();
		return StreamUtils.getReadPeekResult(this, readByte, eofErrorP, eofValue);
	}

	/**
	 * Internal read method for reading a byte from the fileStream.
	 *
	 * @return the next byte in the fileStream
	 */
	private int readByte() {
		try {
			return randomAccessFile.readByte();
		} catch (final EOFException eofe) {
			if (LOGGER.isTraceEnabled()) {
				LOGGER.trace(StreamUtils.END_OF_FILE_REACHED, eofe);
			}
			return -1;
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_READ_BYTE, ioe, this);
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

		return StreamUtils.getReadPeekResult(this, nextChar, eofErrorP, eofValue);
	}

	/**
	 * Attempts to peek ahead to the next available character in the stream.
	 *
	 * @return the character peeked from the stream
	 */
	private int nilPeekCharFSS() {
		try {
			final int nextChar = randomAccessFile.readUnsignedByte();
			randomAccessFile.seek(randomAccessFile.getFilePointer() - 1);
			return nextChar;
		} catch (final EOFException eofe) {
			if (LOGGER.isTraceEnabled()) {
				LOGGER.trace(StreamUtils.END_OF_FILE_REACHED, eofe);
			}
			return -1;
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_PEEK_CHAR, ioe, this);
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

			while (Character.isWhitespace(nextChar) && (nextChar != -1)) {
				nextChar = readChar(StreamUtils.FAILED_TO_PEEK_CHAR);
			}

			if (nextChar != -1) {
				randomAccessFile.seek(randomAccessFile.getFilePointer() - 1);
			}
			return nextChar;
		} catch (final EOFException eofe) {
			if (LOGGER.isTraceEnabled()) {
				LOGGER.trace(StreamUtils.END_OF_FILE_REACHED, eofe);
			}
			return -1;
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
	private int characterPeekCharFSS(final Integer codePoint) {
		try {
			// Initialize to 0 value
			int nextChar = 0;

			while ((nextChar != codePoint) && (nextChar != -1)) {
				nextChar = readChar(StreamUtils.FAILED_TO_PEEK_CHAR);
			}

			if (nextChar != -1) {
				randomAccessFile.seek(randomAccessFile.getFilePointer() - 1);
			}
			return nextChar;
		} catch (final EOFException eofe) {
			if (LOGGER.isTraceEnabled()) {
				LOGGER.trace(StreamUtils.END_OF_FILE_REACHED, eofe);
			}
			return -1;
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_PEEK_CHAR, ioe, this);
		}
	}

	@Override
	public Integer unreadChar(final Integer codePoint) {
		try {
			randomAccessFile.seek(randomAccessFile.getFilePointer() - 1);
			return codePoint;
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_UNREAD_CHAR, ioe, this);
		}
	}

	@Override
	public void clearInput() {
		// Do nothing.
	}

	@Override
	public void writeChar(final int aChar) {
		try {
			randomAccessFile.writeChar(aChar);
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_WRITE_CHAR, ioe, this);
		}
	}

	@Override
	public void writeByte(final int aByte) {
		try {
			randomAccessFile.writeByte(aByte);
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_WRITE_BYTE, ioe, this);
		}
	}

	@Override
	public void writeString(final String outputString, final int start, final int end) {
		try {
			final String subString = outputString.substring(start, end);
			randomAccessFile.writeChars(subString);
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_WRITE_STRING, ioe, this);
		}
	}

	@Override
	public void clearOutput() {
		// Do nothing.
	}

	@Override
	public void finishOutput() {
		// Do nothing.
	}

	@Override
	public void forceOutput() {
		// Do nothing.
	}

	@Override
	public boolean isStartOfLine() {
		final int nextChar = nilPeekCharFSS();
		return nextChar == '\n';
	}

	@Override
	public boolean close() {
		try {
			randomAccessFile.close();
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not close stream.", ioe, this);
		}
		return super.close();
	}

	@Override
	public Long fileLength() {
		try {
			return randomAccessFile.length();
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not retrieve file length.", ioe, this);
		}
	}

	@Override
	public Long filePosition(final Long filePosition) {
		try {
			if (filePosition != null) {
				randomAccessFile.seek(filePosition);
			}
			return randomAccessFile.getFilePointer();
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not retrieve file position.", ioe, this);
		}
	}

	public ExternalFormat getExternalFormat() {
		return ExternalFormat.DEFAULT;
	}
}
