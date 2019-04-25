/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal.stream;

import jcl.lang.BooleanStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.OutputStreamStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.SynonymStreamStruct;
import jcl.lang.TStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.StreamErrorException;
import jcl.lang.internal.VariableStructImpl;
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.stream.PeekType;
import jcl.lang.stream.ReadPeekResult;

/**
 * The {@link SynonymStreamStructImpl} is the object representation of a Lisp 'synonym-stream' type.
 */
public final class SynonymStreamStructImpl extends StreamStructImpl implements SynonymStreamStruct {

	/**
	 * The {@link SymbolStruct} that contains the value for the {@link SymbolStruct} to use.
	 */
	private final SymbolStruct symbol;

	/**
	 * Public constructor.
	 *
	 * @param variable
	 * 		the variable to create a SynonymStreamStruct from
	 */
	public SynonymStreamStructImpl(final VariableStructImpl<?> variable) {
		this(false, variable);
	}

	/**
	 * Public constructor.
	 *
	 * @param symbol
	 * 		the symbol to create a SynonymStreamStruct from
	 */
	public SynonymStreamStructImpl(final SymbolStruct symbol) {
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
	public SynonymStreamStructImpl(final boolean interactive, final SymbolStruct symbol) {
		super(interactive, getElementType(symbol));
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
	private static LispStruct getElementType(final SymbolStruct symbol) {
		if (symbol == null) {
			throw new ErrorException("Provided Symbol must not be null.");
		}
		return ((StreamStructImpl) symbol.getValue()).getElementType();
	}

	/**
	 * Getter for synonym-stream {@link #symbol} property.
	 *
	 * @return synonym-stream {@link #symbol} property
	 */
	@Override
	public SymbolStruct getSymbol() {
		return symbol;
	}

	@Override
	public ReadPeekResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		final StreamStructImpl stream = (StreamStructImpl) symbol.getValue();
		if (stream instanceof InputStreamStruct) {
			return ((InputStreamStruct) stream).readChar(eofErrorP, eofValue, recursiveP);
		} else {
			throw new StreamErrorException("Characters can only be read from Input Streams.", stream);
		}
	}

	@Override
	public ReadPeekResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		final StreamStructImpl stream = (StreamStructImpl) symbol.getValue();
		if (stream instanceof InputStreamStruct) {
			return ((InputStreamStruct) stream).readByte(eofErrorP, eofValue);
		} else {
			throw new StreamErrorException("Bytes can only be read from Input Streams.", stream);
		}
	}

	@Override
	public ReadPeekResult peekChar(final PeekType peekType, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		final StreamStructImpl stream = (StreamStructImpl) symbol.getValue();
		if (stream instanceof InputStreamStruct) {
			return ((InputStreamStruct) stream).peekChar(peekType, eofErrorP, eofValue, recursiveP);
		} else {
			throw new StreamErrorException("Characters can only be peeked at from Input Streams.", stream);
		}
	}

	@Override
	public Integer unreadChar(final Integer codePoint) {
		final StreamStructImpl stream = (StreamStructImpl) symbol.getValue();
		if (stream instanceof InputStreamStruct) {
			return ((InputStreamStruct) stream).unreadChar(codePoint);
		} else {
			throw new StreamErrorException("Characters can only be unread from Input Streams.", stream);
		}
	}

	@Override
	public void clearInput() {
		final StreamStructImpl stream = (StreamStructImpl) symbol.getValue();
		if (stream instanceof InputStreamStruct) {
			((InputStreamStruct) stream).clearInput();
		}
	}

	@Override
	public boolean listen() {
		final StreamStructImpl stream = (StreamStructImpl) symbol.getValue();
		return (stream instanceof InputStreamStruct) && ((InputStreamStruct) stream).listen();
	}

	@Override
	public void writeChar(final int aChar) {
		final StreamStructImpl stream = (StreamStructImpl) symbol.getValue();
		if (stream instanceof OutputStreamStruct) {
			((OutputStreamStruct) stream).writeChar(aChar);
		} else {
			throw new StreamErrorException("Characters can only be written to Input Streams.", stream);
		}
	}

	@Override
	public void writeByte(final int aByte) {
		final StreamStructImpl stream = (StreamStructImpl) symbol.getValue();
		if (stream instanceof OutputStreamStruct) {
			((OutputStreamStruct) stream).writeByte(aByte);
		} else {
			throw new StreamErrorException("Bytes can only be written to Input Streams.", stream);
		}
	}

	@Override
	public void writeString(final String outputString, final int start, final int end) {
		final StreamStructImpl stream = (StreamStructImpl) symbol.getValue();
		if (stream instanceof OutputStreamStruct) {
			((OutputStreamStruct) stream).writeString(outputString, start, end);
		} else {
			throw new StreamErrorException("Strings can only be written to Input Streams.", stream);
		}
	}

	@Override
	public void clearOutput() {
		final StreamStructImpl stream = (StreamStructImpl) symbol.getValue();
		if (stream instanceof OutputStreamStruct) {
			((OutputStreamStruct) stream).clearOutput();
		}
	}

	@Override
	public void finishOutput() {
		final StreamStructImpl stream = (StreamStructImpl) symbol.getValue();
		if (stream instanceof OutputStreamStruct) {
			((OutputStreamStruct) stream).finishOutput();
		}
	}

	@Override
	public void forceOutput() {
		final StreamStructImpl stream = (StreamStructImpl) symbol.getValue();
		if (stream instanceof OutputStreamStruct) {
			((OutputStreamStruct) stream).forceOutput();
		}
	}

	@Override
	public boolean isStartOfLine() {
		final StreamStructImpl stream = (StreamStructImpl) symbol.getValue();
		if (stream instanceof OutputStreamStruct) {
			((OutputStreamStruct) stream).isStartOfLine();
		}
		return false;
	}

	@Override
	public boolean close() {
		final boolean wasClosed = super.close();

		final StreamStructImpl stream = (StreamStructImpl) symbol.getValue();
		final boolean wasSynonymClosed = stream.close();
		return wasClosed || wasSynonymClosed;
	}

	@Override
	public Long fileLength() {
		final StreamStructImpl stream = (StreamStructImpl) symbol.getValue();
		return stream.fileLength();
	}

	@Override
	public Long filePosition(final Long filePosition) {
		final StreamStructImpl stream = (StreamStructImpl) symbol.getValue();
		return stream.filePosition(filePosition);
	}

	@Override
	public LispStruct typeOf() {
		return CommonLispSymbols.SYNONYM_STREAM;
	}

	@Override
	public ClassStruct classOf() {
		return BuiltInClassStruct.SYNONYM_STREAM;
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if (typeSpecifier == CommonLispSymbols.SYNONYM_STREAM) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.SYNONYM_STREAM) {
			return TStruct.INSTANCE;
		}
		return super.typep(typeSpecifier);
	}

	@Override
	public String toString() {
		final String type = typeOf().toString();
		final String printedSymbol = symbol.toString();
		return "#<" + type + " to " + printedSymbol + '>';
	}
}
