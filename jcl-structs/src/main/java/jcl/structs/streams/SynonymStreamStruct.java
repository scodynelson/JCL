package jcl.structs.streams;

import jcl.structs.LispStruct;
import jcl.structs.conditions.exceptions.StreamErrorException;
import jcl.structs.symbols.SymbolStruct;
import jcl.types.LispType;
import jcl.types.streams.SynonymStream;

/**
 * The {@code SynonymStreamStruct} is the object representation of a Lisp 'synonym-stream' type.
 */
public class SynonymStreamStruct extends StreamStruct implements InputStream, OutputStream {

	private final SymbolStruct<StreamStruct> symbol;
	private final StreamStruct stream;

	/**
	 * Public constructor.
	 *
	 * @param symbol the symbol to create a {@code SynonymStreamStruct} from
	 * @throws StreamErrorException if the struct cannot be created
	 */
	public SynonymStreamStruct(final SymbolStruct<StreamStruct> symbol) throws StreamErrorException {
		this(false, symbol);
	}

	/**
	 * Public constructor.
	 *
	 * @param isInteractive whether or not the struct created is 'interactive'
	 * @param symbol        the symbol to create a {@code SynonymStreamStruct} from
	 * @throws StreamErrorException if the struct cannot be created
	 */
	public SynonymStreamStruct(final boolean isInteractive, final SymbolStruct<StreamStruct> symbol) throws StreamErrorException {
		super(SynonymStream.INSTANCE, null, null, isInteractive, getElementType(symbol));
		this.symbol = symbol;
		stream = symbol.getValue();
	}

	/**
	 * This private method is used to retrieve the element type for object construction.
	 *
	 * @param symbol the symbol to create a {@code SynonymStreamStruct} from
	 * @return the element type for object construction
	 * @throws StreamErrorException if the element type cannot be retrieved
	 */
	private static LispType getElementType(final SymbolStruct<StreamStruct> symbol) throws StreamErrorException {
		if (symbol == null) {
			throw new StreamErrorException("Provided Symbol must not be null.");
		}
		return symbol.getValue().elementType();
	}

	public SymbolStruct<StreamStruct> getSymbol() {
		return symbol;
	}

	@Override
	public ReadResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) throws StreamErrorException {
		if (stream instanceof InputStream) {
			return ((InputStream) stream).readChar(eofErrorP, eofValue, recursiveP);
		} else {
			throw new StreamErrorException("Characters can only be read from Input Streams.");
		}
	}

	@Override
	public ReadResult readByte(final boolean eofErrorP, final LispStruct eofValue) throws StreamErrorException {
		if (stream instanceof InputStream) {
			return ((InputStream) stream).readByte(eofErrorP, eofValue);
		} else {
			throw new StreamErrorException("Bytes can only be read from Input Streams.");
		}
	}

	@Override
	public PeekResult peekChar(final PeekType peekType, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) throws StreamErrorException {
		if (stream instanceof InputStream) {
			return ((InputStream) stream).peekChar(peekType, eofErrorP, eofValue, recursiveP);
		} else {
			throw new StreamErrorException("Characters can only be peeked at from Input Streams.");
		}
	}

	@Override
	public Integer unreadChar(final Integer codePoint) throws StreamErrorException {
		if (stream instanceof InputStream) {
			return ((InputStream) stream).unreadChar(codePoint);
		} else {
			throw new StreamErrorException("Characters can only be unread from Input Streams.");
		}
	}

	@Override
	public void writeChar(final int aChar) throws StreamErrorException {
		if (stream instanceof OutputStream) {
			((OutputStream) stream).writeChar(aChar);
		} else {
			throw new StreamErrorException("Characters can only be written to Input Streams.");
		}
	}

	@Override
	public void writeByte(final int aByte) throws StreamErrorException {
		if (stream instanceof OutputStream) {
			((OutputStream) stream).writeByte(aByte);
		} else {
			throw new StreamErrorException("Bytes can only be written to Input Streams.");
		}
	}

	@Override
	public void writeString(final String outputString, final int start, final int end) throws StreamErrorException {
		if (stream instanceof OutputStream) {
			((OutputStream) stream).writeString(outputString, start, end);
		} else {
			throw new StreamErrorException("Strings can only be written to Input Streams.");
		}
	}

	@Override
	public void clearInput() {
		if (stream instanceof InputStream) {
			((InputStream) stream).clearInput();
		}
	}

	@Override
	public boolean listen() {
		return (stream instanceof InputStream) && ((InputStream) stream).listen();
	}

	@Override
	public void clearOutput() {
		if (stream instanceof OutputStream) {
			((OutputStream) stream).clearOutput();
		}
	}

	@Override
	public void finishOutput() {
		if (stream instanceof OutputStream) {
			((OutputStream) stream).finishOutput();
		}
	}

	@Override
	public void forceOutput() {
		if (stream instanceof OutputStream) {
			((OutputStream) stream).forceOutput();
		}
	}

	@Override
	public void close() throws StreamErrorException {
		super.close();
		stream.close();
	}

	@Override
	public Long fileLength() throws StreamErrorException {
		return stream.fileLength();
	}

	@Override
	public Long filePosition(final Long filePosition) throws StreamErrorException {
		return stream.filePosition(filePosition);
	}

	@Override
	public String toString() {
		return "SynonymStreamStruct{" +
				"symbol=" + symbol +
				", stream=" + stream +
				'}';
	}
}
