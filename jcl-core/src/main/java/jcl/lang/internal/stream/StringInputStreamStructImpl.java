/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal.stream;

import jcl.lang.BooleanStruct;
import jcl.lang.CharacterStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ReadCharResult;
import jcl.lang.StringInputStreamStruct;
import jcl.lang.TStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.condition.exception.EndOfFileException;
import jcl.lang.condition.exception.StreamErrorException;
import jcl.lang.statics.CommonLispSymbols;

/**
 * The {@link StringInputStreamStructImpl} is the object representation of a Lisp 'string-stream' input type.
 */
public final class StringInputStreamStructImpl extends StreamStructImpl implements StringInputStreamStruct {

	/**
	 * The {@link String} input value to read characters from.
	 */
	private final String inputString;

	/**
	 * The position to read up to from the {@link #inputString}.
	 */
	private final int end;

	/**
	 * The current location of reads from the {@link #inputString}.
	 */
	private int current;

	/**
	 * Public constructor, initializing the {@link #inputString}, {@link #end}, and {@link #current}.
	 *
	 * @param inputString
	 * 		the input to initialize
	 * @param current
	 * 		the current position to read from in the string
	 * @param end
	 * 		the ending position to read up to in the string
	 */
	public StringInputStreamStructImpl(final String inputString, final int current, final int end) {
		super(CommonLispSymbols.CHARACTER);

		this.inputString = inputString;
		this.end = end;
		this.current = current;
	}

	/*
	INPUT-STREAM-STRUCT
	 */

	@Override
	public ReadCharResult readChar(final boolean eofErrorP, final LispStruct eofValue) {
		if (current == end) {
			if (eofErrorP) {
				throw new EndOfFileException(this);
			} else {
				return new ReadCharResult(eofValue);
			}
		}

		final int readChar = inputString.charAt(current);
		if (readChar == '\n') {
			lineNumber++;
		}
		current++;
		return new ReadCharResult(readChar);
	}

	@Override
	public ReadCharResult readCharNoHang(final boolean eofErrorP, final LispStruct eofValue) {
		return readChar(eofErrorP, eofValue);
	}

	@Override
	public ReadCharResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		throw new StreamErrorException(CharacterStreamStruct.OPERATION_UNSUPPORTED, this);
	}

	@Override
	public Integer unreadChar(final Integer codePoint) {
		if (codePoint == '\n') {
			lineNumber--;
		}
		current--;
		return codePoint;
	}

	@Override
	public BooleanStruct listen() {
		return BooleanStruct.toLispBoolean(current < end);
	}

	/*
	STREAM-STRUCT
	 */

	@Override
	public LispStruct filePosition() {
		return IntegerStruct.toLispInteger(current);
	}

	@Override
	public BooleanStruct filePosition(final IntegerStruct position) {
		current = position.toJavaInt();
		return TStruct.INSTANCE;
	}

	/*
	LISP-STRUCT
	 */

	@Override
	public LispStruct typeOf() {
		return CommonLispSymbols.STRING_INPUT_STREAM;
	}

	@Override
	public ClassStruct classOf() {
		return BuiltInClassStruct.STRING_INPUT_STREAM;
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if (typeSpecifier == CommonLispSymbols.STRING_INPUT_STREAM) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == CommonLispSymbols.STRING_STREAM) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.STRING_INPUT_STREAM) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.STRING_STREAM) {
			return TStruct.INSTANCE;
		}
		return super.typep(typeSpecifier);
	}
}
