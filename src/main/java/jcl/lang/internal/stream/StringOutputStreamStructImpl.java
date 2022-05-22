/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal.stream;

import jcl.lang.BooleanStruct;
import jcl.lang.CharacterStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.StringOutputStreamStruct;
import jcl.lang.StringStruct;
import jcl.lang.TStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.condition.exception.StreamErrorException;
import jcl.lang.statics.CommonLispSymbols;

/**
 * The {@link StringOutputStreamStructImpl} is the object representation of a Lisp 'string-stream' output type.
 */
public final class StringOutputStreamStructImpl extends StreamStructImpl implements StringOutputStreamStruct {

	/**
	 * The {@link StringBuffer} to use for this stream to accept characters and bytes.
	 */
	private final StringBuilder stringBuilder = new StringBuilder();

	/**
	 * Public constructor.
	 *
	 * @param elementType
	 * 		the type of character elements in the stream
	 */
	public StringOutputStreamStructImpl(final LispStruct elementType) {
		super(elementType);
	}

	/*
	STRING-OUTPUT-STREAM-STRUCT
	 */

	@Override
	public StringStruct getOutputStreamString() {
		final String streamString = stringBuilder.toString();
		stringBuilder.setLength(0);
		return StringStruct.toLispString(streamString);
	}

	/*
	OUTPUT-STREAM-STRUCT
	 */

	@Override
	public void writeChar(final int codePoint) {
		stringBuilder.appendCodePoint(codePoint);
	}

	@Override
	public void writeByte(final int aByte) {
		throw new StreamErrorException(CharacterStreamStruct.OPERATION_UNSUPPORTED, this);
	}

	@Override
	public void writeString(final String outputString) {
		stringBuilder.append(outputString);
	}

	@Override
	public void writeLine(final String outputString) {
		stringBuilder.append(outputString);
		stringBuilder.append('\n');
	}

	@Override
	public BooleanStruct freshLine() {
		if (stringBuilder.charAt(stringBuilder.length() - 1) == '\n') {
			return NILStruct.INSTANCE;
		}
		writeChar('\n');
		return TStruct.INSTANCE;
	}

	@Override
	public BooleanStruct terpri() {
		stringBuilder.appendCodePoint('\n');
		return NILStruct.INSTANCE;
	}

	/*
	STREAM-STRUCT
	 */

	@Override
	public LispStruct filePosition() {
		return IntegerStruct.toLispInteger(stringBuilder.length());
	}

	@Override
	public BooleanStruct filePosition(final IntegerStruct position) {
		stringBuilder.setLength(position.toJavaInt());
		return TStruct.INSTANCE;
	}

	/*
	LISP-STRUCT
	 */

	@Override
	public LispStruct typeOf() {
		return CommonLispSymbols.STRING_OUTPUT_STREAM;
	}

	@Override
	public ClassStruct classOf() {
		return BuiltInClassStruct.STRING_OUTPUT_STREAM;
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if (typeSpecifier == CommonLispSymbols.STRING_OUTPUT_STREAM) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == CommonLispSymbols.STRING_STREAM) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.STRING_OUTPUT_STREAM) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.STRING_STREAM) {
			return TStruct.INSTANCE;
		}
		return super.typep(typeSpecifier);
	}
}
