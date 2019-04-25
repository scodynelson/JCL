/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal.stream;

import jcl.lang.BooleanStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.OutputStreamStruct;
import jcl.lang.TStruct;
import jcl.lang.TwoWayStreamStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.statics.CommonLispSymbols;
import jcl.lang.stream.PeekType;
import jcl.lang.stream.ReadPeekResult;

/**
 * The {@link TwoWayStreamStructImpl} is the object representation of a Lisp 'two-way-stream' type.
 */
public final class TwoWayStreamStructImpl extends AbstractDualStreamStructImpl implements TwoWayStreamStruct {

	/**
	 * Public constructor.
	 *
	 * @param inputStreamStruct
	 * 		the {@link InputStreamStruct} to create a TwoWayStreamStruct from
	 * @param outputStreamStruct
	 * 		the {@link OutputStreamStruct} to create a TwoWayStreamStruct from
	 */
	public TwoWayStreamStructImpl(final InputStreamStruct inputStreamStruct, final OutputStreamStruct outputStreamStruct) {
		this(false, inputStreamStruct, outputStreamStruct);
	}

	/**
	 * Public constructor.
	 *
	 * @param interactive
	 * 		whether or not the struct created is 'interactive'
	 * @param inputStreamStruct
	 * 		the {@link InputStreamStruct} to create a TwoWayStreamStruct from
	 * @param outputStreamStruct
	 * 		the {@link OutputStreamStruct} to create a TwoWayStreamStruct from
	 */
	public TwoWayStreamStructImpl(final boolean interactive, final InputStreamStruct inputStreamStruct, final OutputStreamStruct outputStreamStruct) {
		super(interactive, inputStreamStruct, outputStreamStruct);
	}

	@Override
	public ReadPeekResult readChar(final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		return inputStreamStruct.readChar(eofErrorP, eofValue, recursiveP);
	}

	@Override
	public ReadPeekResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		return inputStreamStruct.readByte(eofErrorP, eofValue);
	}

	@Override
	public ReadPeekResult peekChar(final PeekType peekType, final boolean eofErrorP, final LispStruct eofValue, final boolean recursiveP) {
		return inputStreamStruct.peekChar(peekType, eofErrorP, eofValue, recursiveP);
	}

	@Override
	public Integer unreadChar(final Integer codePoint) {
		return inputStreamStruct.unreadChar(codePoint);
	}

	@Override
	public Long filePosition(final Long filePosition) {
		return null;
	}

	@Override
	public LispStruct typeOf() {
		return CommonLispSymbols.TWO_WAY_STREAM;
	}

	@Override
	public ClassStruct classOf() {
		return BuiltInClassStruct.TWO_WAY_STREAM;
	}

	@Override
	public BooleanStruct typep(final LispStruct typeSpecifier) {
		if (typeSpecifier == CommonLispSymbols.TWO_WAY_STREAM) {
			return TStruct.INSTANCE;
		}
		if (typeSpecifier == BuiltInClassStruct.TWO_WAY_STREAM) {
			return TStruct.INSTANCE;
		}
		return super.typep(typeSpecifier);
	}

	@Override
	public String toString() {
		final String type = typeOf().toString();
		final String printedInputStream = inputStreamStruct.toString();
		final String printedOutputStream = outputStreamStruct.toString();
		return "#<" + type + " input " + printedInputStream + ", output " + printedOutputStream + '>';
	}
}
