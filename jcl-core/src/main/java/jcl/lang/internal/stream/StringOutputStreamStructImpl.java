/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal.stream;

import jcl.lang.BooleanStruct;
import jcl.lang.LispStruct;
import jcl.lang.StringOutputStreamStruct;
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
	private final StringBuilder stringBuffer = new StringBuilder();

	/**
	 * Public constructor.
	 */
	public StringOutputStreamStructImpl() {
		this(false);
	}

	/**
	 * Public constructor.
	 *
	 * @param interactive
	 * 		whether or not the struct created is 'interactive'
	 */
	public StringOutputStreamStructImpl(final boolean interactive) {
		this(interactive, CommonLispSymbols.BASE_CHAR);
	}

	/**
	 * Public constructor.
	 *
	 * @param elementType
	 * 		the type of character elements in the stream
	 */
	public StringOutputStreamStructImpl(final LispStruct elementType) {
		this(false, elementType);
	}

	/**
	 * Public constructor.
	 *
	 * @param interactive
	 * 		whether or not the struct created is 'interactive'
	 * @param elementType
	 * 		the type of character elements in the stream
	 */
	public StringOutputStreamStructImpl(final boolean interactive, final LispStruct elementType) {
		super(interactive, elementType);
	}

	@Override
	public String getStreamString() {
		return stringBuffer.toString();
	}

	@Override
	public void clearStream() {
		stringBuffer.setLength(0);
	}

	@Override
	public void writeChar(final int aChar) {
		stringBuffer.appendCodePoint(aChar);
	}

	@Override
	public void writeByte(final int aByte) {
		throw new StreamErrorException(StreamUtils.OPERATION_ONLY_BINARY_STREAM, this);
	}

	@Override
	public void writeString(final String outputString, final int start, final int end) {
		final String subString = outputString.substring(start, end);
		stringBuffer.append(subString);
	}

	@Override
	public void clearOutput() {
		// Do nothing.
	}

	@Override
	public void finishOutput() {
		// Do nothing.
	}

	@Override
	public void forceOutput() {
		// Do nothing.
	}

	@Override
	public boolean isStartOfLine() {
		return stringBuffer.charAt(stringBuffer.length() - 1) == '\n';
	}

	@Override
	public Long fileLength() {
		throw new StreamErrorException(StreamUtils.OPERATION_ONLY_FILE_STREAM, this);
	}

	@Override
	public Long filePosition(final Long filePosition) {
		if (filePosition == null) {
			return (long) stringBuffer.length();
		}
		return null;
	}

	@Override
	public LispStruct typeOf() {
		// TODO
//		return CommonLispSymbols.STRING_OUTPUT_STREAM;
		return CommonLispSymbols.STRING_STREAM;
	}

	@Override
	public ClassStruct classOf() {
		// TODO
//		return BuiltInClassStruct.STRING_OUTPUT_STREAM;
		return BuiltInClassStruct.STRING_STREAM;
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		// TODO
//		if (typeSpecifier == CommonLispSymbols.STRING_OUTPUT_STREAM) {
//			return TStruct.INSTANCE;
//		}
		if (typeSpecifier == CommonLispSymbols.STRING_STREAM) {
			return TStruct.INSTANCE;
		}
		// TODO
//		if (typeSpecifier == BuiltInClassStruct.STRING_OUTPUT_STREAM) {
//			return TStruct.INSTANCE;
//		}
		if (typeSpecifier == BuiltInClassStruct.STRING_STREAM) {
			return TStruct.INSTANCE;
		}
		// TODO
		return super.typep(typeSpecifier);
	}
}
