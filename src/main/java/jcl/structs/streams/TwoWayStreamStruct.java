package jcl.structs.streams;

import jcl.LispStruct;
import jcl.reader.syntax.reader.PeekResult;
import jcl.reader.syntax.reader.PeekType;
import jcl.reader.syntax.reader.ReadResult;
import jcl.types.TwoWayStream;

/**
 * The {@link TwoWayStreamStruct} is the object representation of a Lisp 'two-way-stream' type.
 */
public class TwoWayStreamStruct extends DualStreamStruct {

	/**
	 * Public constructor.
	 *
	 * @param inputStream  the {@link InputStream} to create a TwoWayStreamStruct from
	 * @param outputStream the {@link OutputStream} to create a TwoWayStreamStruct from
	 */
	public TwoWayStreamStruct(final InputStream inputStream, final OutputStream outputStream) {
		this(false, inputStream, outputStream);
	}

	/**
	 * Public constructor.
	 *
	 * @param isInteractive whether or not the struct created is 'interactive'
	 * @param inputStream   the {@link InputStream} to create a TwoWayStreamStruct from
	 * @param outputStream  the {@link OutputStream} to create a TwoWayStreamStruct from
	 */
	public TwoWayStreamStruct(final boolean isInteractive, final InputStream inputStream, final OutputStream outputStream) {
		super(TwoWayStream.INSTANCE, isInteractive, inputStream, outputStream);
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
	public Long filePosition(final Long filePosition) {
		return null;
	}

	@Override
	protected String getPrintableObjectProperties() {
		return " input " + inputStream.printStruct() + ", output " + outputStream.printStruct();
	}

	@Override
	public String toString() {
		return "TwoWayStreamStruct{"
				+ "inputStream=" + inputStream
				+ ", outputStream=" + outputStream
				+ '}';
	}
}
