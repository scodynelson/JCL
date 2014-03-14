package jcl.structs;

import jcl.LispStruct;
import jcl.structs.conditions.exceptions.EndOfFileException;
import jcl.structs.conditions.exceptions.StreamErrorException;
import jcl.structs.streams.PeekResult;
import jcl.structs.streams.PeekType;
import jcl.structs.streams.ReadResult;
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
public class FileStreamStruct extends StreamStruct implements InputStream, OutputStream {

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
		super(FileStream.INSTANCE, null, null, isInteractive, null);
		// TODO: Type will be the type of whatever the "byte" type being read

		try {
			fileStream = new RandomAccessFile(file, "rw");
		} catch (final FileNotFoundException fnfe) {
			throw new StreamErrorException("Failed to open provided file.", fnfe);
		}
	}

	@Override
	public ReadResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		int readChar;
		try {
			readChar = fileStream.read();
		} catch (final EOFException eofe) {
			LOGGER.warn("End of file reached.", eofe);
			readChar = -1;
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not read char.", ioe);
		}

		if (readChar == -1) {
			if (eofErrorP) {
				throw new EndOfFileException("End of file reached.");
			} else {
				return new ReadResult(eofValue);
			}
		} else {
			return new ReadResult(readChar);
		}
	}

	@Override
	public ReadResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		int readByte;
		try {
			readByte = fileStream.readByte();
		} catch (final EOFException eofe) {
			LOGGER.warn("End of file reached.", eofe);
			readByte = -1;
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not read byte.", ioe);
		}

		if (readByte == -1) {
			if (eofErrorP) {
				throw new EndOfFileException("End of file reached.");
			} else {
				return new ReadResult(eofValue);
			}
		} else {
			return new ReadResult(readByte);
		}
	}

	@Override
	public PeekResult peekChar(final PeekType peekType, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {

		int nextChar = -1;
		switch (peekType.getType()) {
			case NIL:
				nextChar = nilPeekChar();
				break;
			case T:
				nextChar = tPeekChar();
				break;
			case CHARACTER:
				nextChar = characterPeekChar(peekType.getCodePoint());
				break;
		}

		if (nextChar == -1) {
			if (eofErrorP) {
				throw new EndOfFileException("End of file reached.");
			} else {
				return new PeekResult(eofValue);
			}
		} else {
			return new PeekResult(nextChar);
		}
	}

	/**
	 * This method attempts to peek ahead to the next available character in the stream.
	 *
	 * @return the character peeked from the stream
	 */
	private int nilPeekChar() {
		int nextChar;
		try {
			nextChar = fileStream.readChar();
			fileStream.seek(fileStream.getFilePointer() - 1);
		} catch (final EOFException eofe) {
			LOGGER.warn("End of file reached.", eofe);
			nextChar = -1;
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not peek char", ioe);
		}
		return nextChar;
	}

	/**
	 * This method attempts to peek ahead to the next available non-whitespace character in the stream.
	 *
	 * @return the character peeked from the stream
	 */
	private int tPeekChar() {
		int nextChar = ' '; // Initialize to whitespace, since we are attempting to skip it anyways
		try {
			int i = 0;
			while (Character.isWhitespace(nextChar)) {
				nextChar = fileStream.readChar();
				i++;
			}
			fileStream.seek(fileStream.getFilePointer() - i);
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not peek char", ioe);
		}
		return nextChar;
	}

	/**
	 * This method attempts to peek ahead to the provided codePoint in the stream.
	 *
	 * @param codePoint the codePoint to peek up to in the stream
	 * @return the character peeked from the stream
	 */
	private int characterPeekChar(final Integer codePoint) {
		int nextChar = -1; // Initialize to -1 value, since this is essentially EOF
		try {
			int i = 0;
			while (nextChar != codePoint) {
				nextChar = fileStream.readChar();
				i++;
			}
			fileStream.seek(fileStream.getFilePointer() - i);
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not peek char", ioe);
		}
		return nextChar;
	}

	@Override
	public Integer unreadChar(final Integer codePoint) {
		try {
			fileStream.seek(fileStream.getFilePointer() - 1);
			return codePoint;
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not unread char.", ioe);
		}
	}

	@Override
	public boolean listen() {
		try {
			final PeekResult peekResult = peekChar(PeekType.NIL_PEEK_TYPE, false, null, false);
			return !peekResult.wasEOF();
		} catch (final EndOfFileException eofe) {
			LOGGER.warn("End of file reached.", eofe);
			return false;
		} catch (final StreamErrorException see) {
			LOGGER.warn("Stream error occurred.", see);
			return false;
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
			throw new StreamErrorException("Could not write char.", ioe);
		}
	}

	@Override
	public void writeByte(final int aByte) {
		try {
			fileStream.writeByte(aByte);
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not write byte.", ioe);
		}
	}

	@Override
	public void writeString(final String outputString, final int start, final int end) {
		try {
			final String subString = outputString.substring(start, end);
			fileStream.writeChars(subString);
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not write string.", ioe);
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
