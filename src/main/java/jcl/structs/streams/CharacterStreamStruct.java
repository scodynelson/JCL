package jcl.structs.streams;

import jcl.LispStruct;
import jcl.structs.conditions.exceptions.StreamErrorException;
import jcl.syntax.reader.PeekResult;
import jcl.syntax.reader.PeekType;
import jcl.syntax.reader.ReadResult;
import jcl.types.Character;
import jcl.types.Stream;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.nio.charset.Charset;

/**
 * The {@link CharacterStreamStruct} is the object representation of a character reading system level Lisp stream.
 */
public class CharacterStreamStruct extends NativeStreamStruct {

	private final LineNumberReader inputStream;
	private final PrintWriter outputStream;

	private static final Logger LOGGER = LoggerFactory.getLogger(CharacterStreamStruct.class);

	/**
	 * Public constructor.
	 *
	 * @param inputStream  the {@link java.io.InputStream} to create a CharacterStreamStruct from
	 * @param outputStream the {@link java.io.OutputStream} to create a CharacterStreamStruct from
	 */
	public CharacterStreamStruct(final InputStream inputStream, final OutputStream outputStream) {
		this(false, inputStream, outputStream);
	}

	/**
	 * Public constructor.
	 *
	 * @param isInteractive whether or not the struct created is 'interactive'
	 * @param inputStream   the {@link java.io.InputStream} to create a CharacterStreamStruct from
	 * @param outputStream  the {@link java.io.OutputStream} to create a CharacterStreamStruct from
	 */
	public CharacterStreamStruct(final boolean isInteractive, final InputStream inputStream, final OutputStream outputStream) {
		super(Stream.INSTANCE, isInteractive, Character.INSTANCE);
		this.inputStream = new LineNumberReader(new InputStreamReader(inputStream, Charset.defaultCharset()));
		this.outputStream = new PrintWriter(new OutputStreamWriter(outputStream, Charset.defaultCharset()));
	}

	@Override
	public ReadResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		try {
			inputStream.mark(1);
			final int readChar = inputStream.read();
			return StreamUtils.getReadResult(readChar, eofErrorP, eofValue);
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_READ_CHAR, ioe);
		}
	}

	@Override
	public ReadResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		throw new StreamErrorException(StreamUtils.OPERATION_ONLY_BINARYSTREAM);
	}

	@Override
	public PeekResult peekChar(final PeekType peekType, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {

		final int nextChar;
		switch (peekType.getType()) {
			case NIL:
				nextChar = nilPeekCharCSS();
				break;
			case T:
				nextChar = tPeekCharCSS();
				break;
			case CHARACTER:
				nextChar = characterPeekCharCSS(peekType.getCodePoint());
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
	private int nilPeekCharCSS() {
		try {
			inputStream.mark(1);
			final int nextChar = inputStream.read();
			inputStream.reset();
			return nextChar;
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_PEEK_CHAR, ioe);
		}
	}

	/**
	 * This method attempts to peek ahead to the next available non-whitespace character in the stream.
	 *
	 * @return the character peeked from the stream
	 */
	private int tPeekCharCSS() {
		try {
			inputStream.mark(1);
			int nextChar = ' '; // Initialize to whitespace, since we are attempting to skip it anyways
			while (java.lang.Character.isWhitespace(nextChar)) {
				nextChar = inputStream.read();
			}
			inputStream.reset();
			return nextChar;
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
	private int characterPeekCharCSS(final Integer codePoint) {
		try {
			inputStream.mark(1);
			int nextChar = -1; // Initialize to -1 value, since this is essentially EOF
			while (nextChar != codePoint) {
				nextChar = inputStream.read();
			}
			inputStream.reset();
			return nextChar;
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_PEEK_CHAR, ioe);
		}
	}

	@Override
	public Integer unreadChar(final Integer codePoint) {
		try {
			inputStream.reset();
			return codePoint;
		} catch (final IOException ioe) {
			throw new StreamErrorException(StreamUtils.FAILED_TO_UNREAD_CHAR, ioe);
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
		throw new StreamErrorException(StreamUtils.OPERATION_ONLY_BINARYSTREAM);
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
		throw new StreamErrorException(StreamUtils.OPERATION_ONLY_FILESTREAM);
	}

	@Override
	public Long filePosition(final Long filePosition) {
		return null;
	}

	@Override
	public String toString() {
		// TODO: fix toString warning...
		return "CharacterStreamStruct{"
				+ "inputStream=" + inputStream
				+ ", outputStream=" + outputStream
				+ '}';
	}
}
