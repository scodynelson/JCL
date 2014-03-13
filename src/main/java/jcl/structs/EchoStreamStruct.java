package jcl.structs;

import jcl.structs.conditions.exceptions.EndOfFileException;
import jcl.structs.conditions.exceptions.StreamErrorException;
import jcl.structs.streams.PeekResult;
import jcl.structs.streams.PeekType;
import jcl.structs.streams.ReadResult;
import jcl.types.LispType;
import jcl.types.EchoStream;
import jcl.typespecifiers.compound.AndTypeSpecifier;

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
	 */
	public EchoStreamStruct(final InputStream inputStream, final OutputStream outputStream) {
		this(false, inputStream, outputStream);
	}

	/**
	 * Public constructor.
	 *
	 * @param isInteractive whether or not the struct created is 'interactive'
	 * @param inputStream   the {@code InputStream} to create a {@code EchoStreamStruct} from
	 * @param outputStream  the {@code OutputStream} to create a {@code EchoStreamStruct} from
	 */
	public EchoStreamStruct(final boolean isInteractive, final InputStream inputStream, final OutputStream outputStream) {
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
	 */
	private static LispType getElementType(final InputStream inputStream, final OutputStream outputStream) {
		if (inputStream == null) {
			throw new StreamErrorException("Provided Input Stream must not be null.");
		}
		if (outputStream == null) {
			throw new StreamErrorException("Provided Output Stream must not be null.");
		}

		final LispType inType = inputStream.getElementType();
		final LispType outType = outputStream.getElementType();

		if (inType.equals(outType)) {
			return inType;
		} else {
			return new AndTypeSpecifier(inType, outType);
		}
	}

	@Override
	public ReadResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
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
	public ReadResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
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
	public PeekResult peekChar(final PeekType peekType, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
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
	public void writeChar(final int aChar) {
		outputStream.writeChar(aChar);
	}

	@Override
	public void writeByte(final int aByte) {
		outputStream.writeByte(aByte);
	}

	@Override
	public void writeString(final String outputString, final int start, final int end) {
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
	public Long fileLength() {
		throw new StreamErrorException("Operation only supported on a FileStream.");
	}

	@Override
	public Long filePosition(final Long filePosition) {
		return null;
	}

	@Override
	public String toString() {
		return "EchoStreamStruct{"
				+ "inputStream=" + inputStream
				+ ", outputStream=" + outputStream
				+ ", unreadStuff=" + unreadStuff
				+ '}';
	}
}
