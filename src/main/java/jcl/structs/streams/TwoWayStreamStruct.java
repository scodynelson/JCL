package jcl.structs.streams;

import jcl.structs.LispStruct;
import jcl.structs.conditions.exceptions.StreamErrorException;
import jcl.types.LispType;
import jcl.types.TwoWayStream;
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
	 */
	public TwoWayStreamStruct(final InputStream inputStream, final OutputStream outputStream) {
		this(false, inputStream, outputStream);
	}

	/**
	 * Public constructor.
	 *
	 * @param isInteractive whether or not the struct created is 'interactive'
	 * @param inputStream   the {@code InputStream} to create a {@code TwoWayStreamStruct} from
	 * @param outputStream  the {@code OutputStream} to create a {@code TwoWayStreamStruct} from
	 */
	public TwoWayStreamStruct(final boolean isInteractive, final InputStream inputStream, final OutputStream outputStream) {
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
		return inputStream.readChar(eofErrorP, eofValue, recursiveP);
	}

	@Override
	public ReadResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		return inputStream.readByte(eofErrorP, eofValue);
	}

	@Override
	public PeekResult peekChar(final PeekType peekType, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		return inputStream.peekChar(peekType, eofErrorP, eofValue, recursiveP);
	}

	@Override
	public Integer unreadChar(final Integer codePoint) {
		return inputStream.unreadChar(codePoint);
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
	public Long fileLength() {
		throw new StreamErrorException("Operation only supported on a FileStream.");
	}

	@Override
	public Long filePosition(final Long filePosition) {
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
