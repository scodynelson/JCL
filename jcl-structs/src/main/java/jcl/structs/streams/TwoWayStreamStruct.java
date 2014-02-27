package jcl.structs.streams;

import jcl.structs.LispStruct;
import jcl.structs.conditions.exceptions.StreamErrorException;
import jcl.types.LispType;
import jcl.types.streams.TwoWayStream;
import jcl.types.typespecifiers.compound.AndTypeSpecifier;

/**
 * The {@code TwoWayStreamStruct} is the object representation of a Lisp 'two-way-stream' type.
 */
public class TwoWayStreamStruct extends StreamStruct implements InputStream, OutputStream {

	private final InputStream inputStream;
	private final OutputStream outputStream;

	/**
	 * Public constructor.
	 *
	 * @param inputStream  the {@code InputStream} to create a {@code TwoWayStreamStruct} from
	 * @param outputStream the {@code OutputStream} to create a {@code TwoWayStreamStruct} from
	 * @throws StreamErrorException if the struct cannot be created
	 */
	public TwoWayStreamStruct(final InputStream inputStream, final OutputStream outputStream) throws StreamErrorException {
		this(false, inputStream, outputStream);
	}

	/**
	 * Public constructor.
	 *
	 * @param isInteractive whether or not the struct created is 'interactive'
	 * @param inputStream   the {@code InputStream} to create a {@code TwoWayStreamStruct} from
	 * @param outputStream  the {@code OutputStream} to create a {@code TwoWayStreamStruct} from
	 * @throws StreamErrorException if the struct cannot be created
	 */
	public TwoWayStreamStruct(final boolean isInteractive, final InputStream inputStream, final OutputStream outputStream) throws StreamErrorException {
		super(TwoWayStream.INSTANCE, null, null, isInteractive, getElementType(inputStream, outputStream));
		this.inputStream = inputStream;
		this.outputStream = outputStream;
	}

	/**
	 * This private method is used to retrieve the element type for object construction.
	 *
	 * @param inputStream  the {@code InputStream} to create a {@code TwoWayStreamStruct} from
	 * @param outputStream the {@code OutputStream} to create a {@code TwoWayStreamStruct} from
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
		return inputStream.readChar(eofErrorP, eofValue, recursiveP);
	}

	@Override
	public ReadResult readByte(final boolean eofErrorP, final LispStruct eofValue) throws StreamErrorException {
		return inputStream.readByte(eofErrorP, eofValue);
	}

	@Override
	public PeekResult peekChar(final PeekType peekType, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) throws StreamErrorException {
		return inputStream.peekChar(peekType, eofErrorP, eofValue, recursiveP);
	}

	@Override
	public Integer unreadChar(final Integer codePoint) throws StreamErrorException {
		return inputStream.unreadChar(codePoint);
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
		return inputStream.listen();
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
		return "TwoWayStreamStruct{"
				+ "inputStream=" + inputStream
				+ ", outputStream=" + outputStream
				+ '}';
	}
}
