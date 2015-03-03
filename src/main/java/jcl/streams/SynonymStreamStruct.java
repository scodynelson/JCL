/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.streams;

import jcl.LispStruct;
import jcl.LispType;
import jcl.conditions.exceptions.StreamErrorException;
import jcl.symbols.SymbolStruct;
import jcl.types.SynonymStream;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link SynonymStreamStruct} is the object representation of a Lisp 'synonym-stream' type.
 */
public class SynonymStreamStruct extends StreamStruct implements InputStream, OutputStream {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -3809768442993249585L;

	/**
	 * The {@link SymbolStruct} that contains the value for the {@link StreamStruct} to use.
	 */
	private final SymbolStruct<StreamStruct> symbol;

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
	 * @param interactive
	 * 		whether or not the struct created is 'interactive'
	 * @param symbol
	 * 		the symbol to create a SynonymStreamStruct from
	 */
	public SynonymStreamStruct(final boolean interactive, final SymbolStruct<StreamStruct> symbol) {
		super(SynonymStream.INSTANCE, null, null, interactive, getElementType(symbol));
		this.symbol = symbol;
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
	public ReadPeekResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		final StreamStruct stream = symbol.getValue();
		if (stream instanceof InputStream) {
			return ((InputStream) stream).readChar(eofErrorP, eofValue, recursiveP);
		} else {
			throw new StreamErrorException("Characters can only be read from Input Streams.");
		}
	}

	@Override
	public ReadPeekResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		final StreamStruct stream = symbol.getValue();
		if (stream instanceof InputStream) {
			return ((InputStream) stream).readByte(eofErrorP, eofValue);
		} else {
			throw new StreamErrorException("Bytes can only be read from Input Streams.");
		}
	}

	@Override
	public ReadPeekResult peekChar(final PeekType peekType, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		final StreamStruct stream = symbol.getValue();
		if (stream instanceof InputStream) {
			return ((InputStream) stream).peekChar(peekType, eofErrorP, eofValue, recursiveP);
		} else {
			throw new StreamErrorException("Characters can only be peeked at from Input Streams.");
		}
	}

	@Override
	public Integer unreadChar(final Integer codePoint) {
		final StreamStruct stream = symbol.getValue();
		if (stream instanceof InputStream) {
			return ((InputStream) stream).unreadChar(codePoint);
		} else {
			throw new StreamErrorException("Characters can only be unread from Input Streams.");
		}
	}

	@Override
	public void clearInput() {
		final StreamStruct stream = symbol.getValue();
		if (stream instanceof InputStream) {
			((InputStream) stream).clearInput();
		}
	}

	@Override
	public boolean listen() {
		final StreamStruct stream = symbol.getValue();
		return (stream instanceof InputStream) && ((InputStream) stream).listen();
	}

	@Override
	public void writeChar(final int aChar) {
		final StreamStruct stream = symbol.getValue();
		if (stream instanceof OutputStream) {
			((OutputStream) stream).writeChar(aChar);
		} else {
			throw new StreamErrorException("Characters can only be written to Input Streams.");
		}
	}

	@Override
	public void writeByte(final int aByte) {
		final StreamStruct stream = symbol.getValue();
		if (stream instanceof OutputStream) {
			((OutputStream) stream).writeByte(aByte);
		} else {
			throw new StreamErrorException("Bytes can only be written to Input Streams.");
		}
	}

	@Override
	public void writeString(final String outputString, final int start, final int end) {
		final StreamStruct stream = symbol.getValue();
		if (stream instanceof OutputStream) {
			((OutputStream) stream).writeString(outputString, start, end);
		} else {
			throw new StreamErrorException("Strings can only be written to Input Streams.");
		}
	}

	@Override
	public void clearOutput() {
		final StreamStruct stream = symbol.getValue();
		if (stream instanceof OutputStream) {
			((OutputStream) stream).clearOutput();
		}
	}

	@Override
	public void finishOutput() {
		final StreamStruct stream = symbol.getValue();
		if (stream instanceof OutputStream) {
			((OutputStream) stream).finishOutput();
		}
	}

	@Override
	public void forceOutput() {
		final StreamStruct stream = symbol.getValue();
		if (stream instanceof OutputStream) {
			((OutputStream) stream).forceOutput();
		}
	}

	@Override
	public void close() {
		super.close();

		final StreamStruct stream = symbol.getValue();
		stream.close();
	}

	@Override
	public Long fileLength() {
		final StreamStruct stream = symbol.getValue();
		return stream.fileLength();
	}

	@Override
	public Long filePosition(final Long filePosition) {
		final StreamStruct stream = symbol.getValue();
		return stream.filePosition(filePosition);
	}

	@Override
	public int hashCode() {
		return HashCodeBuilder.reflectionHashCode(this);
	}

	@Override
	public boolean equals(final Object obj) {
		return EqualsBuilder.reflectionEquals(this, obj);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
