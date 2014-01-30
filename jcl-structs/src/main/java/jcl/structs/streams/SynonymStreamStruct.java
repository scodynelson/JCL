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

	private final SymbolStruct<LispStream> symbol;
	private final LispStream stream;

	private final boolean isInteractive;
	private boolean isClosed;

	/**
	 * Private constructor.
	 *
	 * @param isInteractive whether or not the struct created is 'interactive'
	 * @param symbol        the symbol to create a {@code SynonymStreamStruct} from
	 * @throws StreamErrorException if the struct cannot be created
	 */
	private SynonymStreamStruct(final boolean isInteractive, final SymbolStruct<LispStream> symbol) throws StreamErrorException {
		this.isInteractive = isInteractive;

		if (symbol == null) {
			throw new StreamErrorException("Provided Symbol must not be null.");
		}
		this.symbol = symbol;
		stream = symbol.getValue();
	}

	public SymbolStruct<LispStream> getSymbol() {
		return symbol;
	}

	@Override
	public LispType getType() {
		return SynonymStream.INSTANCE;
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
	public PeekResult peekChar(final LispType peekType, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) throws StreamErrorException {
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
		isClosed = true;
		stream.close();
	}

	@Override
	public LispType elementType() {
		return stream.elementType();
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
	public boolean isInteractive() {
		return !isClosed && isInteractive;
	}

	@Override
	public boolean isClosed() {
		return isClosed;
	}

	@Override
	public String toString() {
		return "SynonymStreamStruct{" +
				"symbol=" + symbol +
				", stream=" + stream +
				", isInteractive=" + isInteractive +
				", isClosed=" + isClosed +
				'}';
	}

	// BUILDERS

	/**
	 * This method gets the {@code SynonymStreamStruct} for the provided {@code symbol}.
	 *
	 * @param symbol the symbol to create a {@code SynonymStreamStruct} from
	 * @return the created {@code SynonymStreamStruct}
	 * @throws StreamErrorException if the struct cannot be created
	 */
	public static SynonymStreamStruct getStruct(final SymbolStruct<LispStream> symbol) throws StreamErrorException {
		return new SynonymStreamStruct(false, symbol);
	}

	/**
	 * This method gets the {@code SynonymStreamStruct} for the provided {@code symbol}.
	 *
	 * @param isInteractive whether or not the struct created is 'interactive'
	 * @param symbol        the symbol to create a {@code SynonymStreamStruct} from
	 * @return the created {@code SynonymStreamStruct}
	 * @throws StreamErrorException if the struct cannot be created
	 */
	public static SynonymStreamStruct getStruct(final boolean isInteractive, final SymbolStruct<LispStream> symbol) throws StreamErrorException {
		return new SynonymStreamStruct(isInteractive, symbol);
	}
}
