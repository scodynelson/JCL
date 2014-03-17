package jcl.streams;

import jcl.LispStruct;
import jcl.conditions.exceptions.StreamErrorException;
import jcl.syntax.reader.PeekResult;
import jcl.syntax.reader.PeekType;
import jcl.syntax.reader.ReadResult;
import jcl.types.FileStream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.EOFException;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;

/**
 * The {@code FileStreamStruct} is the object representation of a Lisp 'file-stream' type.
 */
public class FileStreamStruct extends NativeStreamStruct {

	private final RandomAccessFile fileStream;

	private static final Logger LOGGER = LoggerFactory.getLogger(FileStreamStruct.class);

	/**
	 * Public constructor.
	 *
	 * @param file the file to create a {@code FileStreamStruct} from
	 */
	public FileStreamStruct(final File file) {
		this(false, file);
	}

	/**
	 * Public constructor.
	 *
	 * @param isInteractive whether or not the struct created is 'interactive'
	 * @param file          the file to create a {@code FileStreamStruct} from
	 */
	public FileStreamStruct(final boolean isInteractive, final File file) {
		super(FileStream.INSTANCE, isInteractive, null);
		// TODO: Type will be the type of whatever the "byte" type being read

		try {
			fileStream = new RandomAccessFile(file, "rw");
		} catch (final FileNotFoundException fnfe) {
			throw new StreamErrorException("Failed to open provided file.", fnfe);
		}
	}

	@Override
	public ReadResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		final int readChar = readChar();
		return StreamUtils.getReadResult(readChar, eofErrorP, eofValue);
	}

	/**
	 * Internal read method for reading a character from the fileStream.
	 *
	 * @return the next character in the fileStream
	 */
	private int readChar() {
		try {
			return fileStream.read();
		} catch (final EOFException eofe) {
			LOGGER.warn(StreamUtils.END_OF_FILE_REACHED, eofe);
			return -1;
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_READ_CHAR, ioe);
		}
	}

	@Override
	public ReadResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		final int readByte = readByte();
		return StreamUtils.getReadResult(readByte, eofErrorP, eofValue);
	}

	/**
	 * Internal read method for reading a byte from the fileStream.
	 *
	 * @return the next byte in the fileStream
	 */
	private int readByte() {
		try {
			return fileStream.readByte();
		} catch (final EOFException eofe) {
			LOGGER.warn(StreamUtils.END_OF_FILE_REACHED, eofe);
			return -1;
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_READ_BYTE, ioe);
		}
	}

	@Override
	public PeekResult peekChar(final PeekType peekType, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {

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

		return StreamUtils.getPeekResult(nextChar, eofErrorP, eofValue);
	}

	/**
	 * This method attempts to peek ahead to the next available character in the stream.
	 *
	 * @return the character peeked from the stream
	 */
	private int nilPeekCharFSS() {
		try {
			final int nextChar = fileStream.readChar();
			fileStream.seek(fileStream.getFilePointer() - 1);
			return nextChar;
		} catch (final EOFException eofe) {
			LOGGER.warn(StreamUtils.END_OF_FILE_REACHED, eofe);
			return -1;
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_PEEK_CHAR, ioe);
		}
	}

	/**
	 * This method attempts to peek ahead to the next available non-whitespace character in the stream.
	 *
	 * @return the character peeked from the stream
	 */
	private int tPeekCharFSS() {
		try {
			int nextChar = ' '; // Initialize to whitespace, since we are attempting to skip it anyways

			int i = 0;
			while (Character.isWhitespace(nextChar)) {
				nextChar = fileStream.readChar();
				i++;
			}

			fileStream.seek(fileStream.getFilePointer() - i);
			return nextChar;
		} catch (final EOFException eofe) {
			LOGGER.warn(StreamUtils.END_OF_FILE_REACHED, eofe);
			return -1;
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_PEEK_CHAR, ioe);
		}
	}

	/**
	 * This method attempts to peek ahead to the provided codePoint in the stream.
	 *
	 * @param codePoint the codePoint to peek up to in the stream
	 * @return the character peeked from the stream
	 */
	private int characterPeekCharFSS(final Integer codePoint) {
		try {
			int nextChar = -1; // Initialize to -1 value, since this is essentially EOF

			int i = 0;
			while (nextChar != codePoint) {
				nextChar = fileStream.readChar();
				i++;
			}

			fileStream.seek(fileStream.getFilePointer() - i);
			return nextChar;
		} catch (final EOFException eofe) {
			LOGGER.warn(StreamUtils.END_OF_FILE_REACHED, eofe);
			return -1;
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_PEEK_CHAR, ioe);
		}
	}

	@Override
	public Integer unreadChar(final Integer codePoint) {
		try {
			fileStream.seek(fileStream.getFilePointer() - 1);
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
			fileStream.writeChar(aChar);
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_WRITE_CHAR, ioe);
		}
	}

	@Override
	public void writeByte(final int aByte) {
		try {
			fileStream.writeByte(aByte);
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_WRITE_BYTE, ioe);
		}
	}

	@Override
	public void writeString(final String outputString, final int start, final int end) {
		try {
			final String subString = outputString.substring(start, end);
			fileStream.writeChars(subString);
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_WRITE_STRING, ioe);
		}
	}

	@Override
	public void forceOutput() {
		// Do nothing.
	}

	@Override
	public void finishOutput() {
		// Do nothing.
	}

	@Override
	public void clearOutput() {
		// Do nothing.
	}

	@Override
	public void close() {
		try {
			fileStream.close();
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not close stream.", ioe);
		}
		super.close();
	}

	@Override
	public Long fileLength() {
		try {
			return fileStream.length();
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not retrieve file length.", ioe);
		}
	}

	@Override
	public Long filePosition(final Long filePosition) {
		try {
			if (filePosition != null) {
				fileStream.seek(filePosition);
			}
			return fileStream.getFilePointer();
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not retrieve file position.", ioe);
		}
	}

	@Override
	public String toString() {
		return "FileStreamStruct{"
				+ "fileStream=" + fileStream
				+ '}';
	}
}
