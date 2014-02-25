package jcl.structs.streams;

import jcl.structs.LispStruct;
import jcl.structs.conditions.exceptions.EndOfFileException;
import jcl.structs.conditions.exceptions.StreamErrorException;
import jcl.types.LispType;
import jcl.types.streams.EchoStream;
import jcl.types.typespecifiers.compound.AndTypeSpecifier;

import java.util.LinkedList;

/**
 * The {@code EchoStreamStruct} is the object representation of a Lisp 'echo-stream' type.
 */
public class EchoStreamStruct extends StreamStruct implements InputStream, OutputStream {

	private final InputStream inputStream;
	private final OutputStream outputStream;

	private final LinkedList<Integer> unreadStuff = new LinkedList<>();

	/**
	 * Public constructor.
	 *
	 * @param inputStream  the {@code InputStream} to create a {@code EchoStreamStruct} from
	 * @param outputStream the {@code OutputStream} to create a {@code EchoStreamStruct} from
	 * @throws StreamErrorException if the struct cannot be created
	 */
	public EchoStreamStruct(final InputStream inputStream, final OutputStream outputStream) throws StreamErrorException {
		this(false, inputStream, outputStream);
	}

	/**
	 * Public constructor.
	 *
	 * @param isInteractive whether or not the struct created is 'interactive'
	 * @param inputStream   the {@code InputStream} to create a {@code EchoStreamStruct} from
	 * @param outputStream  the {@code OutputStream} to create a {@code EchoStreamStruct} from
	 * @throws StreamErrorException if the struct cannot be created
	 */
	public EchoStreamStruct(final boolean isInteractive, final InputStream inputStream, final OutputStream outputStream) throws StreamErrorException {
		super(EchoStream.INSTANCE, null, null, isInteractive, getElementType(inputStream, outputStream));
		this.inputStream = inputStream;
		this.outputStream = outputStream;
	}

	/**
	 * This private method is used to retrieve the element type for object construction.
	 *
	 * @param inputStream  the {@code InputStream} to create a {@code EchoStreamStruct} from
	 * @param outputStream the {@code OutputStream} to create a {@code EchoStreamStruct} from
	 * @return the element type for object construction
	 * @throws StreamErrorException if the element type cannot be retrieved
	 */
	private static LispType getElementType(final InputStream inputStream, final OutputStream outputStream) throws StreamErrorException {
		if (inputStream == null) {
			throw new StreamErrorException("Provided Input Stream must not be null.");
		}
		if (outputStream == null) {
			throw new StreamErrorException("Provided Output Stream must not be null.");
		}

		final LispType inType = inputStream.elementType();
		final LispType outType = outputStream.elementType();

		if (inType.equals(outType)) {
			return inType;
		} else {
			return new AndTypeSpecifier(inType, outType);
		}
	}

	@Override
	public ReadResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) throws StreamErrorException {
		if (!unreadStuff.isEmpty()) {
			final Integer lastUnread = unreadStuff.getFirst();
			return new ReadResult(lastUnread);
		}

		final ReadResult readResult = inputStream.readChar(false, eofValue, false);

		if (readResult.wasEOF()) {
			if (eofErrorP) {
				throw new EndOfFileException("End of file reached.");
			} else {
				return readResult;
			}
		} else {
			final int readChar = readResult.getResult();
			outputStream.writeChar(readChar);
			return readResult;
		}
	}

	@Override
	public ReadResult readByte(final boolean eofErrorP, final LispStruct eofValue) throws StreamErrorException {
		if (!unreadStuff.isEmpty()) {
			final Integer lastUnread = unreadStuff.getFirst();
			return new ReadResult(lastUnread);
		}

		final ReadResult readResult = inputStream.readByte(false, eofValue);

		if (readResult.wasEOF()) {
			if (eofErrorP) {
				throw new EndOfFileException("End of file reached.");
			} else {
				return readResult;
			}
		} else {
			final int readByte = readResult.getResult();
			outputStream.writeByte(readByte);
			return readResult;
		}
	}

	@Override
	public PeekResult peekChar(final LispType peekType, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) throws StreamErrorException {
		if (unreadStuff.isEmpty()) {
			final ReadResult readResult = inputStream.readChar(eofErrorP, eofValue, recursiveP);

			if (readResult.wasEOF()) {
				return new PeekResult(readResult.getEofValue());
			} else {
				final int peekedChar = readResult.getResult();
				outputStream.writeChar(peekedChar);
				return new PeekResult(peekedChar);
			}
		} else {
			final Integer peekedChar = unreadStuff.removeFirst();
			return new PeekResult(peekedChar);
		}
	}

	@Override
	public Integer unreadChar(final Integer codePoint) {
		unreadStuff.addFirst(codePoint);
		return codePoint;
	}

	@Override
	public void writeChar(final int aChar) throws StreamErrorException {
		outputStream.writeChar(aChar);
	}

	@Override
	public void writeByte(final int aByte) throws StreamErrorException {
		outputStream.writeByte(aByte);
	}

	@Override
	public void writeString(final String outputString, final int start, final int end) throws StreamErrorException {
		outputStream.writeString(outputString, start, end);
	}

	@Override
	public void clearInput() {
		inputStream.clearInput();
	}

	@Override
	public boolean listen() {
		return !unreadStuff.isEmpty() || inputStream.listen();
	}

	@Override
	public void clearOutput() {
		outputStream.clearOutput();
	}

	@Override
	public void finishOutput() {
		outputStream.finishOutput();
	}

	@Override
	public void forceOutput() {
		outputStream.forceOutput();
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
		return "EchoStreamStruct{" +
				"inputStream=" + inputStream +
				", outputStream=" + outputStream +
				", unreadStuff=" + unreadStuff +
				'}';
	}
}
