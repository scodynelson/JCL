package jcl.structs.streams;

import jcl.structs.LispStruct;
import jcl.structs.conditions.exceptions.EndOfFileException;
import jcl.structs.conditions.exceptions.StreamErrorException;
import jcl.types.LispType;
import jcl.types.characters.Character;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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

	private static final Logger LOGGER = LoggerFactory.getLogger(CharacterStreamStruct.class);

	/**
	 * Public constructor.
	 *
	 * @param inputStream  the {@code java.io.InputStream} to create a {@code CharacterStreamStruct} from
	 * @param outputStream the {@code java.io.OutputStream} to create a {@code CharacterStreamStruct} from
	 */
	public CharacterStreamStruct(final java.io.InputStream inputStream, final java.io.OutputStream outputStream) {
		this(false, inputStream, outputStream);
	}

	/**
	 * Public constructor.
	 *
	 * @param isInteractive whether or not the struct created is 'interactive'
	 * @param inputStream   the {@code java.io.InputStream} to create a {@code CharacterStreamStruct} from
	 * @param outputStream  the {@code java.io.OutputStream} to create a {@code CharacterStreamStruct} from
	 */
	public CharacterStreamStruct(final boolean isInteractive, final java.io.InputStream inputStream, final java.io.OutputStream outputStream) {
		super(null, null, isInteractive, Character.INSTANCE);
		this.inputStream = new LineNumberReader(new InputStreamReader(inputStream));
		this.outputStream = new PrintWriter(new OutputStreamWriter(outputStream));
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
		try {
			inputStream.mark(0);
			inputStream.reset();
		} catch (final IOException ioe) {
			LOGGER.warn("IO exception occurred.", ioe);
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
		super.close();
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
	public String toString() {
		return "CharacterStreamStruct{" +
				"inputStream=" + inputStream +
				", outputStream=" + outputStream +
				'}';
	}
}
