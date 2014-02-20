package jcl.structs.streams;

import jcl.structs.LispStruct;
import jcl.structs.conditions.exceptions.EndOfFileException;
import jcl.structs.conditions.exceptions.StreamErrorException;
import jcl.types.LispType;
import jcl.types.characters.BaseChar;
import jcl.types.streams.StringStream;

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
	 * @throws StreamErrorException if the struct cannot be created
	 */
	public StringInputStreamStruct(final String inputString) throws StreamErrorException {
		this(false, inputString);
	}

	/**
	 * Public constructor.
	 *
	 * @param isInteractive whether or not the struct created is 'interactive'
	 * @param inputString   the input to create a {@code StringInputStreamStruct} from
	 * @throws StreamErrorException if the struct cannot be created
	 */
	public StringInputStreamStruct(final boolean isInteractive, final String inputString) throws StreamErrorException {
		super(StringStream.INSTANCE, null, null, isInteractive, BaseChar.INSTANCE);

		if (inputString == null) {
			throw new StreamErrorException("Provided Input String must not be null.");
		}
		this.inputString = inputString;

		end = inputString.length();
		current = 0;
	}

	@Override
	public ReadResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) throws StreamErrorException {
		if (current == end) {
			if (eofErrorP) {
				throw new EndOfFileException("End of file reached.");
			} else {
				return new ReadResult(eofValue);
			}
		}

		final int readChar = inputString.charAt(current++);
		return new ReadResult(readChar);
	}

	@Override
	public ReadResult readByte(final boolean eofErrorP, final LispStruct eofValue) throws StreamErrorException {
		throw new StreamErrorException("Operation only supported for BinaryStreams.");
	}

	@Override
	public PeekResult peekChar(final LispType peekType, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) throws StreamErrorException {
		if ((current + 1) == end) {
			if (eofErrorP) {
				throw new EndOfFileException("End of file reached.");
			} else {
				return new PeekResult(eofValue);
			}
		}

		final int nextChar = inputString.charAt(current + 1);
		return new PeekResult(nextChar);
	}

	@Override
	public Integer unreadChar(final Integer codePoint) throws StreamErrorException {
		current--;
		return codePoint;
	}

	@Override
	public void clearInput() {
	}

	@Override
	public boolean listen() {
		return current < end;
	}

	@Override
	public Long fileLength() throws StreamErrorException {
		throw new StreamErrorException("Operation only supported on a FileStream.");
	}

	@Override
	public Long filePosition(final Long filePosition) throws StreamErrorException {
		if (filePosition != null) {
			current = filePosition.intValue();
		}
		return (long) current;
	}

	@Override
	public String toString() {
		return "StringInputStreamStruct{" +
				"inputString='" + inputString + '\'' +
				", end=" + end +
				", current=" + current +
				'}';
	}
}
