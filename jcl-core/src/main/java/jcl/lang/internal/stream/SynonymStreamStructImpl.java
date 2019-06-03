/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal.stream;

import jcl.lang.BooleanStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.OutputStreamStruct;
import jcl.lang.StreamStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.SynonymStreamStruct;
import jcl.lang.TStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.condition.exception.StreamErrorException;
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.stream.ReadCharResult;

/**
 * The {@link SynonymStreamStructImpl} is the object representation of a Lisp 'synonym-stream' type.
 */
public final class SynonymStreamStructImpl extends StreamStructImpl implements SynonymStreamStruct {

	/**
	 * The {@link SymbolStruct} that contains the {@link StreamStruct} value to delegate to for the stream operations.
	 */
	private final SymbolStruct symbol;

	/**
	 * Public constructor, initializing the provided {@link SymbolStruct}.
	 *
	 * @param symbol
	 * 		the {@link SymbolStruct} to initialize
	 */
	public SynonymStreamStructImpl(final SymbolStruct symbol) {
		super(getElementType(symbol));
		this.symbol = symbol;
	}

	/**
	 * Retrieves the current element type for stream.
	 *
	 * @param symbol
	 *        {@link SymbolStruct} that contains the {@link StreamStruct} value to delegate to
	 *
	 * @return the current element type for stream
	 */
	private static LispStruct getElementType(final SymbolStruct symbol) {
		return ((StreamStruct) symbol.getValue()).streamElementType();
	}

	/*
	SYNONYM-STREAM-STRUCT
	 */

	@Override
	public SymbolStruct synonymStreamSymbol() {
		return symbol;
	}

	/*
	INPUT-STREAM-STRUCT
	 */

	@Override
	public ReadCharResult readChar(final boolean eofErrorP, final LispStruct eofValue) {
		final StreamStruct stream = (StreamStruct) symbol.getValue();
		if (stream instanceof InputStreamStruct) {
			return ((InputStreamStruct) stream).readChar(eofErrorP, eofValue);
		} else {
			throw new StreamErrorException("Characters can only be read from Input Streams.", stream);
		}
	}

	@Override
	public ReadCharResult readCharNoHang(final boolean eofErrorP, final LispStruct eofValue) {
		final StreamStruct stream = (StreamStruct) symbol.getValue();
		if (stream instanceof InputStreamStruct) {
			return ((InputStreamStruct) stream).readCharNoHang(eofErrorP, eofValue);
		} else {
			throw new StreamErrorException("Characters can only be read from Input Streams.", stream);
		}
	}

	@Override
	public ReadCharResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		final StreamStruct stream = (StreamStruct) symbol.getValue();
		if (stream instanceof InputStreamStruct) {
			return ((InputStreamStruct) stream).readByte(eofErrorP, eofValue);
		} else {
			throw new StreamErrorException("Bytes can only be read from Input Streams.", stream);
		}
	}

	@Override
	public Integer unreadChar(final Integer codePoint) {
		final StreamStruct stream = (StreamStruct) symbol.getValue();
		if (stream instanceof InputStreamStruct) {
			return ((InputStreamStruct) stream).unreadChar(codePoint);
		} else {
			throw new StreamErrorException("Characters can only be unread from Input Streams.", stream);
		}
	}

	@Override
	public LispStruct clearInput() {
		final StreamStruct stream = (StreamStruct) symbol.getValue();
		if (stream instanceof InputStreamStruct) {
			((InputStreamStruct) stream).clearInput();
		}
		return NILStruct.INSTANCE;
	}

	@Override
	public BooleanStruct listen() {
		final StreamStruct stream = (StreamStruct) symbol.getValue();
		if (stream instanceof InputStreamStruct) {
			return ((InputStreamStruct) stream).listen();
		}
		return NILStruct.INSTANCE;
	}

	/*
	OUTPUT-STREAM-STRUCT
	 */

	@Override
	public void writeChar(final int codePoint) {
		final StreamStruct stream = (StreamStruct) symbol.getValue();
		if (stream instanceof OutputStreamStruct) {
			((OutputStreamStruct) stream).writeChar(codePoint);
		} else {
			throw new StreamErrorException("Characters can only be written to Input Streams.", stream);
		}
	}

	@Override
	public void writeByte(final int aByte) {
		final StreamStruct stream = (StreamStruct) symbol.getValue();
		if (stream instanceof OutputStreamStruct) {
			((OutputStreamStruct) stream).writeByte(aByte);
		} else {
			throw new StreamErrorException("Bytes can only be written to Input Streams.", stream);
		}
	}

	@Override
	public void writeString(final String outputString) {
		final StreamStruct stream = (StreamStruct) symbol.getValue();
		if (stream instanceof OutputStreamStruct) {
			((OutputStreamStruct) stream).writeString(outputString);
		} else {
			throw new StreamErrorException("Strings can only be written to Input Streams.", stream);
		}
	}

	@Override
	public void writeLine(final String outputString) {
		final StreamStruct stream = (StreamStruct) symbol.getValue();
		if (stream instanceof OutputStreamStruct) {
			((OutputStreamStruct) stream).writeLine(outputString);
		} else {
			throw new StreamErrorException("Strings can only be written to Input Streams.", stream);
		}
	}

	@Override
	public BooleanStruct freshLine() {
		final StreamStruct stream = (StreamStruct) symbol.getValue();
		if (stream instanceof OutputStreamStruct) {
			return ((OutputStreamStruct) stream).freshLine();
		}
		return NILStruct.INSTANCE;
	}

	@Override
	public BooleanStruct terpri() {
		final StreamStruct stream = (StreamStruct) symbol.getValue();
		if (stream instanceof OutputStreamStruct) {
			return ((OutputStreamStruct) stream).terpri();
		}
		return NILStruct.INSTANCE;
	}

	@Override
	public LispStruct clearOutput() {
		final StreamStruct stream = (StreamStruct) symbol.getValue();
		if (stream instanceof OutputStreamStruct) {
			((OutputStreamStruct) stream).clearOutput();
		}
		return NILStruct.INSTANCE;
	}

	@Override
	public LispStruct finishOutput() {
		final StreamStruct stream = (StreamStruct) symbol.getValue();
		if (stream instanceof OutputStreamStruct) {
			((OutputStreamStruct) stream).finishOutput();
		}
		return NILStruct.INSTANCE;
	}

	@Override
	public LispStruct forceOutput() {
		final StreamStruct stream = (StreamStruct) symbol.getValue();
		if (stream instanceof OutputStreamStruct) {
			((OutputStreamStruct) stream).forceOutput();
		}
		return NILStruct.INSTANCE;
	}

	/*
	STREAM-STRUCT
	 */

	@Override
	public BooleanStruct close(final BooleanStruct abort) {
		final boolean wasClosed = super.close(abort).toJavaPBoolean();

		final StreamStruct stream = (StreamStruct) symbol.getValue();
		final boolean wasSynonymClosed = stream.close(abort).toJavaPBoolean();

		return BooleanStruct.toLispBoolean(wasClosed || wasSynonymClosed);
	}

	@Override
	public LispStruct fileLength() {
		final StreamStruct stream = (StreamStruct) symbol.getValue();
		return stream.fileLength();
	}

	@Override
	public LispStruct filePosition() {
		final StreamStruct stream = (StreamStruct) symbol.getValue();
		return stream.filePosition();
	}

	@Override
	public BooleanStruct filePosition(final IntegerStruct position) {
		final StreamStruct stream = (StreamStruct) symbol.getValue();
		return stream.filePosition(position);
	}

	/*
	LISP-STRUCT
	 */

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
		return "#<" + type + " to " + symbol + '>';
	}
}
