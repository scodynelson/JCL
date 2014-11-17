package jcl.structs.streams;

import jcl.LispStruct;
import jcl.LispType;
import jcl.reader.syntax.reader.PeekResult;
import jcl.reader.syntax.reader.PeekType;
import jcl.reader.syntax.reader.ReadResult;
import jcl.structs.conditions.exceptions.StreamErrorException;
import jcl.structs.symbols.SymbolStruct;
import jcl.types.SynonymStream;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link SynonymStreamStruct} is the object representation of a Lisp 'synonym-stream' type.
 */
public class SynonymStreamStruct extends StreamStruct implements InputStream, OutputStream {

	private final SymbolStruct<StreamStruct> symbol;
	private final StreamStruct stream;

	/**
	 * Public constructor.
	 *
	 * @param symbol
	 * 		the symbol to create a SynonymStreamStruct from
	 */
	public SynonymStreamStruct(final SymbolStruct<StreamStruct> symbol) {
		this(false, symbol);
	}

	/**
	 * Public constructor.
	 *
	 * @param isInteractive
	 * 		whether or not the struct created is 'interactive'
	 * @param symbol
	 * 		the symbol to create a SynonymStreamStruct from
	 */
	public SynonymStreamStruct(final boolean isInteractive, final SymbolStruct<StreamStruct> symbol) {
		super(SynonymStream.INSTANCE, null, null, isInteractive, getElementType(symbol));
		this.symbol = symbol;
		stream = symbol.getValue();
	}

	/**
	 * Retrieves the element type for object construction.
	 *
	 * @param symbol
	 * 		the symbol to create a SynonymStreamStruct from
	 *
	 * @return the element type for object construction
	 */
	private static LispType getElementType(final SymbolStruct<StreamStruct> symbol) {
		if (symbol == null) {
			throw new StreamErrorException("Provided Symbol must not be null.");
		}
		return symbol.getValue().getElementType();
	}

	/**
	 * Getter for synonym-stream {@link #symbol} property.
	 *
	 * @return synonym-stream {@link #symbol} property
	 */
	public SymbolStruct<StreamStruct> getSymbol() {
		return symbol;
	}

	@Override
	public ReadResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		if (stream instanceof InputStream) {
			return ((InputStream) stream).readChar(eofErrorP, eofValue, recursiveP);
		} else {
			throw new StreamErrorException("Characters can only be read from Input Streams.");
		}
	}

	@Override
	public ReadResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		if (stream instanceof InputStream) {
			return ((InputStream) stream).readByte(eofErrorP, eofValue);
		} else {
			throw new StreamErrorException("Bytes can only be read from Input Streams.");
		}
	}

	@Override
	public PeekResult peekChar(final PeekType peekType, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		if (stream instanceof InputStream) {
			return ((InputStream) stream).peekChar(peekType, eofErrorP, eofValue, recursiveP);
		} else {
			throw new StreamErrorException("Characters can only be peeked at from Input Streams.");
		}
	}

	@Override
	public Integer unreadChar(final Integer codePoint) {
		if (stream instanceof InputStream) {
			return ((InputStream) stream).unreadChar(codePoint);
		} else {
			throw new StreamErrorException("Characters can only be unread from Input Streams.");
		}
	}

	@Override
	public void writeChar(final int aChar) {
		if (stream instanceof OutputStream) {
			((OutputStream) stream).writeChar(aChar);
		} else {
			throw new StreamErrorException("Characters can only be written to Input Streams.");
		}
	}

	@Override
	public void writeByte(final int aByte) {
		if (stream instanceof OutputStream) {
			((OutputStream) stream).writeByte(aByte);
		} else {
			throw new StreamErrorException("Bytes can only be written to Input Streams.");
		}
	}

	@Override
	public void writeString(final String outputString, final int start, final int end) {
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
	public void close() {
		super.close();
		stream.close();
	}

	@Override
	public Long fileLength() {
		return stream.fileLength();
	}

	@Override
	public Long filePosition(final Long filePosition) {
		return stream.filePosition(filePosition);
	}

	@Override
	protected String getPrintableObjectProperties() {
		return " to " + symbol.printStruct();
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
