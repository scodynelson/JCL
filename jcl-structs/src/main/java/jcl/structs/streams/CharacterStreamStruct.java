package jcl.structs.streams;

import jcl.structs.LispStruct;
import jcl.structs.conditions.exceptions.EndOfFileException;
import jcl.structs.conditions.exceptions.StreamErrorException;
import jcl.types.LispType;
import jcl.types.characters.Character;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;

/**
 * The {@code CharacterStreamStruct} is the object representation of a character reading system level Lisp stream.
 */
public class CharacterStreamStruct extends StreamStruct implements InputStream, OutputStream {

	private final LineNumberReader inputStream;
	private final PrintWriter outputStream;

	private final boolean isInteractive;
	private boolean isClosed;

	/**
	 * Private constructor.
	 *
	 * @param isInteractive whether or not the struct created is 'interactive'
	 * @param inputStream   the {@code java.io.InputStream} to create a {@code CharacterStreamStruct} from
	 * @param outputStream  the {@code java.io.OutputStream} to create a {@code CharacterStreamStruct} from
	 */
	private CharacterStreamStruct(final boolean isInteractive, final java.io.InputStream inputStream, final java.io.OutputStream outputStream) {
		this.isInteractive = isInteractive;
		this.inputStream = new LineNumberReader(new InputStreamReader(inputStream));
		this.outputStream = new PrintWriter(new OutputStreamWriter(outputStream));
	}

	@Override
	public LispType getType() {
		return null;
	}

	@Override
	public ReadResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) throws StreamErrorException {
		final int readChar;
		try {
			inputStream.mark(1);
			readChar = inputStream.read();
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
		throw new StreamErrorException("Operation only supported for BinaryStreams.");
	}

	@Override
	public PeekResult peekChar(final LispType peekType, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) throws StreamErrorException {
		final int nextChar;
		try {
			inputStream.mark(1);
			nextChar = inputStream.read();
			inputStream.reset();
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
			inputStream.reset();
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
		try {
			inputStream.mark(0);
			inputStream.reset();
		} catch (final IOException ignored) {
		}
	}

	@Override
	public void writeChar(final int aChar) throws StreamErrorException {
		outputStream.append((char) aChar);
	}

	@Override
	public void writeByte(final int aByte) throws StreamErrorException {
		throw new StreamErrorException("Operation only supported for BinaryStreams.");
	}

	@Override
	public void writeString(final String outputString, final int start, final int end) throws StreamErrorException {
		outputStream.append(outputString, start, end);
	}

	@Override
	public void forceOutput() {
		outputStream.flush();
	}

	@Override
	public void finishOutput() {
		outputStream.flush();
	}

	@Override
	public void clearOutput() {
		outputStream.flush();
	}

	@Override
	public void close() throws StreamErrorException {
		try {
			inputStream.close();
			outputStream.close();
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not close stream.", ioe);
		}
		isClosed = true;
	}

	@Override
	public LispType elementType() {
		return Character.INSTANCE;
	}

	@Override
	public Long fileLength() throws StreamErrorException {
		throw new StreamErrorException("Operation only supported on a FileStream.");
	}

	@Override
	public Long filePosition(final Long filePosition) throws StreamErrorException {
		return null;
	}

	@Override
	public boolean isInteractive() {
		return !isClosed && isInteractive;
	}

	@Override
	public boolean isClosed() {
		return isClosed;
	}

	@Override
	public String toString() {
		return "CharacterStreamStruct{" +
				"inputStream=" + inputStream +
				", outputStream=" + outputStream +
				", isInteractive=" + isInteractive +
				", isClosed=" + isClosed +
				'}';
	}

	// BUILDERS

	/**
	 * This method gets the {@code CharacterStreamStruct} for the provided {@code inputStream} and {@code outputStream}.
	 *
	 * @param inputStream  the {@code java.io.InputStream} to create a {@code CharacterStreamStruct} from
	 * @param outputStream the {@code java.io.OutputStream} to create a {@code CharacterStreamStruct} from
	 * @return the created {@code CharacterStreamStruct}
	 */
	public static CharacterStreamStruct getStruct(final java.io.InputStream inputStream, final java.io.OutputStream outputStream) {
		return new CharacterStreamStruct(false, inputStream, outputStream);
	}

	/**
	 * This method gets the {@code CharacterStreamStruct} for the provided {@code inputStream} and {@code outputStream}.
	 *
	 * @param isInteractive whether or not the struct created is 'interactive'
	 * @param inputStream   the {@code java.io.InputStream} to create a {@code CharacterStreamStruct} from
	 * @param outputStream  the {@code java.io.OutputStream} to create a {@code CharacterStreamStruct} from
	 * @return the created {@code CharacterStreamStruct}
	 */
	public static CharacterStreamStruct getStruct(final boolean isInteractive, final java.io.InputStream inputStream, final java.io.OutputStream outputStream) {
		return new CharacterStreamStruct(isInteractive, inputStream, outputStream);
	}
}
