package jcl.structs.streams;

import jcl.structs.LispStruct;
import jcl.structs.conditions.exceptions.EndOfFileException;
import jcl.structs.conditions.exceptions.StreamErrorException;
import jcl.types.characters.Character;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.nio.charset.Charset;

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
		this.inputStream = new LineNumberReader(new InputStreamReader(inputStream, Charset.defaultCharset()));
		this.outputStream = new PrintWriter(new OutputStreamWriter(outputStream, Charset.defaultCharset()));
	}

	@Override
	public ReadResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
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
	public ReadResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		throw new StreamErrorException("Operation only supported for BinaryStreams.");
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
		final int nextChar;
		try {
			inputStream.mark(1);
			nextChar = inputStream.read();
			inputStream.reset();
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
			inputStream.mark(1);
			while (java.lang.Character.isWhitespace(nextChar)) {
				nextChar = inputStream.read();
			}
			inputStream.reset();
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
			inputStream.mark(1);
			while (nextChar != codePoint) {
				nextChar = inputStream.read();
			}
			inputStream.reset();
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not peek char", ioe);
		}
		return nextChar;
	}

	@Override
	public Integer unreadChar(final Integer codePoint) {
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
		try {
			inputStream.mark(0);
			inputStream.reset();
		} catch (final IOException ioe) {
			LOGGER.warn("IO exception occurred.", ioe);
		}
	}

	@Override
	public void writeChar(final int aChar) {
		outputStream.append((char) aChar);
	}

	@Override
	public void writeByte(final int aByte) {
		throw new StreamErrorException("Operation only supported for BinaryStreams.");
	}

	@Override
	public void writeString(final String outputString, final int start, final int end) {
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
	public void close() {
		try {
			inputStream.close();
			outputStream.close();
		} catch (final IOException ioe) {
			throw new StreamErrorException("Could not close stream.", ioe);
		}
		super.close();
	}

	@Override
	public Long fileLength() {
		throw new StreamErrorException("Operation only supported on a FileStream.");
	}

	@Override
	public Long filePosition(final Long filePosition) {
		return null;
	}

	@Override
	public String toString() {
		return "CharacterStreamStruct{"
				+ "inputStream=" + inputStream
				+ ", outputStream=" + outputStream
				+ '}';
	}
}
