package jcl.structs.streams;

import jcl.structs.LispStruct;
import jcl.structs.conditions.exceptions.EndOfFileException;
import jcl.structs.conditions.exceptions.StreamErrorException;
import jcl.types.LispType;
import jcl.types.streams.FileStream;

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

	/**
	 * Public constructor.
	 *
	 * @param file the file to create a {@code FileStreamStruct} from
	 * @throws StreamErrorException if the struct cannot be created
	 */
	public FileStreamStruct(final File file) throws StreamErrorException {
		this(false, file);
	}

	/**
	 * Public constructor.
	 *
	 * @param isInteractive whether or not the struct created is 'interactive'
	 * @param file          the file to create a {@code FileStreamStruct} from
	 * @throws StreamErrorException if the struct cannot be created
	 */
	public FileStreamStruct(final boolean isInteractive, final File file) throws StreamErrorException {
		super(FileStream.INSTANCE, null, null, isInteractive, null);
		// TODO: Type will be the type of whatever the "byte" type being read

		try {
			fileStream = new RandomAccessFile(file, "rw");
		} catch (final FileNotFoundException fnfe) {
			throw new StreamErrorException("Failed to open provided file.", fnfe);
		}
	}

	@Override
	public ReadResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) throws StreamErrorException {
		int readChar;
		try {
			readChar = fileStream.read();
		} catch (final EOFException ignored) {
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
	public ReadResult readByte(final boolean eofErrorP, final LispStruct eofValue) throws StreamErrorException {
		int readByte;
		try {
			readByte = fileStream.readByte();
		} catch (final EOFException ignored) {
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
	public PeekResult peekChar(final LispType peekType, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) throws StreamErrorException {
		int nextChar;
		try {
			nextChar = fileStream.readChar();
			fileStream.seek(fileStream.getFilePointer() - 1);
		} catch (final EOFException ignored) {
			nextChar = -1;
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not peek char", ioe);
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

	@Override
	public Integer unreadChar(final Integer codePoint) throws StreamErrorException {
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
			final PeekResult peekResult = peekChar(null, false, null, false);
			return !peekResult.wasEOF();
		} catch (final EndOfFileException ignored) {
			return false;
		} catch (final StreamErrorException ignored) {
			return false;
		}
	}

	@Override
	public void clearInput() {
	}

	@Override
	public void writeChar(final int aChar) throws StreamErrorException {
		try {
			fileStream.writeChar(aChar);
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not write char.", ioe);
		}
	}

	@Override
	public void writeByte(final int aByte) throws StreamErrorException {
		try {
			fileStream.writeByte(aByte);
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not write byte.", ioe);
		}
	}

	@Override
	public void writeString(final String outputString, final int start, final int end) throws StreamErrorException {
		try {
			final String subString = outputString.substring(start, end);
			fileStream.writeChars(subString);
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not write string.", ioe);
		}
	}

	@Override
	public void forceOutput() {
	}

	@Override
	public void finishOutput() {
	}

	@Override
	public void clearOutput() {
	}

	@Override
	public void close() throws StreamErrorException {
		try {
			fileStream.close();
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not close stream.", ioe);
		}
		super.close();
	}

	@Override
	public Long fileLength() throws StreamErrorException {
		try {
			return fileStream.length();
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not retrieve file length.", ioe);
		}
	}

	@Override
	public Long filePosition(final Long filePosition) throws StreamErrorException {
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
		return "FileStreamStruct{" +
				"fileStream=" + fileStream +
				'}';
	}
}
