package jcl.streams;

import jcl.LispStruct;
import jcl.syntax.reader.PeekResult;
import jcl.syntax.reader.PeekType;
import jcl.syntax.reader.ReadResult;
import jcl.types.TwoWayStream;

/**
 * The {@code TwoWayStreamStruct} is the object representation of a Lisp 'two-way-stream' type.
 */
public class TwoWayStreamStruct extends DualStreamStruct {

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
	public String toString() {
		return "TwoWayStreamStruct{"
				+ "inputStream=" + inputStream
				+ ", outputStream=" + outputStream
				+ '}';
	}
}