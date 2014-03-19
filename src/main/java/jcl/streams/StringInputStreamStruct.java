package jcl.streams;

import jcl.LispStruct;
import jcl.conditions.exceptions.EndOfFileException;
import jcl.conditions.exceptions.StreamErrorException;
import jcl.syntax.reader.PeekResult;
import jcl.syntax.reader.PeekType;
import jcl.syntax.reader.ReadResult;
import jcl.types.BaseChar;
import jcl.types.StringStream;

/**
 * The {@code StringInputStreamStruct} is the object representation of a Lisp 'string-stream' input type.
 */
public class StringInputStreamStruct extends StreamStruct implements InputStream {

	private final String inputString;
	private final int end;
	private int current;

	/**
	 * Public constructor.
	 *
	 * @param inputString the input to create a {@code StringInputStreamStruct} from
	 */
	public StringInputStreamStruct(final String inputString) {
		this(false, inputString);
	}

	/**
	 * Public constructor.
	 *
	 * @param isInteractive whether or not the struct created is 'interactive'
	 * @param inputString   the input to create a {@code StringInputStreamStruct} from
	 */
	public StringInputStreamStruct(final boolean isInteractive, final String inputString) {
		super(StringStream.INSTANCE, null, null, isInteractive, BaseChar.INSTANCE);

		if (inputString == null) {
			throw new StreamErrorException("Provided Input String must not be null.");
		}
		this.inputString = inputString;

		end = inputString.length();
		current = 0;
	}

	@Override
	public ReadResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		if (current == end) {
			if (eofErrorP) {
				throw new EndOfFileException(StreamUtils.END_OF_FILE_REACHED);
			} else {
				return new ReadResult(eofValue);
			}
		}

		final int readChar = inputString.charAt(current++);
		return new ReadResult(readChar);
	}

	@Override
	public ReadResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		throw new StreamErrorException(StreamUtils.OPERATION_ONLY_BINARYSTREAM);
	}

	@Override
	public PeekResult peekChar(final PeekType peekType, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		if ((current + 1) == end) {
			if (eofErrorP) {
				throw new EndOfFileException(StreamUtils.END_OF_FILE_REACHED);
			} else {
				return new PeekResult(eofValue);
			}
		}

		final int nextChar;
		switch (peekType.getType()) {
			case NIL:
				nextChar = nilPeekCharSIS();
				break;
			case T:
				nextChar = tPeekCharSIS();
				break;
			case CHARACTER:
				nextChar = characterPeekCharSIS(peekType.getCodePoint());
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
	private int nilPeekCharSIS() {
		return inputString.charAt(current + 1);
	}

	/**
	 * This method attempts to peek ahead to the next available non-whitespace character in the stream.
	 *
	 * @return the character peeked from the stream
	 */
	private int tPeekCharSIS() {
		int nextChar = ' '; // Initialize to whitespace, since we are attempting to skip it anyways

		int indexToFind = current + 1;
		while (Character.isWhitespace(nextChar) && (indexToFind < end)) {
			nextChar = inputString.charAt(current + indexToFind);
			indexToFind++;
		}

		return (indexToFind == end) ? nextChar : -1;
	}

	/**
	 * This method attempts to peek ahead to the provided codePoint in the stream.
	 *
	 * @param codePoint the codePoint to peek up to in the stream
	 * @return the character peeked from the stream
	 */
	private int characterPeekCharSIS(final Integer codePoint) {
		int nextChar = -1; // Initialize to -1 value, since this is essentially EOF

		int indexToFind = current + 1;
		while ((nextChar != codePoint) && (indexToFind < end)) {
			nextChar = inputString.charAt(current + indexToFind);
			indexToFind++;
		}

		return (indexToFind == end) ? nextChar : -1;
	}

	@Override
	public Integer unreadChar(final Integer codePoint) {
		current--;
		return codePoint;
	}

	@Override
	public void clearInput() {
		// Do nothing.
	}

	@Override
	public boolean listen() {
		return current < end;
	}

	@Override
	public Long fileLength() {
		throw new StreamErrorException(StreamUtils.OPERATION_ONLY_FILESTREAM);
	}

	@Override
	public Long filePosition(final Long filePosition) {
		if (filePosition != null) {
			current = filePosition.intValue();
		}
		return (long) current;
	}

	@Override
	public String toString() {
		return "StringInputStreamStruct{"
				+ "inputString='" + inputString + '\''
				+ ", end=" + end
				+ ", current=" + current
				+ '}';
	}
}
