/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal.stream;

import jcl.lang.BooleanStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.OutputStreamStruct;
import jcl.lang.ReadCharResult;
import jcl.lang.TStruct;
import jcl.lang.TwoWayStreamStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.classes.ClassStruct;
import jcl.lang.statics.CommonLispSymbols;

/**
 * The {@link TwoWayStreamStructImpl} is the object representation of a Lisp 'two-way-stream' type.
 */
public final class TwoWayStreamStructImpl extends AbstractDualStreamStructImpl implements TwoWayStreamStruct {

	/**
	 * Public constructor, initializing the provided {@link InputStreamStruct} and {@link OutputStreamStruct}.
	 *
	 * @param inputStreamStruct
	 * 		the {@link InputStreamStruct} to initialize
	 * @param outputStreamStruct
	 * 		the {@link OutputStreamStruct} to initialize
	 */
	public TwoWayStreamStructImpl(final InputStreamStruct inputStreamStruct,
	                              final OutputStreamStruct outputStreamStruct) {
		super(inputStreamStruct, outputStreamStruct);
	}

	/*
	TWO-WAY-STREAM-STRUCT
	 */

	@Override
	public InputStreamStruct twoWayStreamInputStream() {
		return inputStreamStruct;
	}

	@Override
	public OutputStreamStruct twoWayStreamOutputStream() {
		return outputStreamStruct;
	}

	/*
	INPUT-STREAM-STRUCT
	 */

	@Override
	public ReadCharResult readChar(final boolean eofErrorP, final LispStruct eofValue) {
		return inputStreamStruct.readChar(eofErrorP, eofValue);
	}

	@Override
	public ReadCharResult readCharNoHang(final boolean eofErrorP, final LispStruct eofValue) {
		return inputStreamStruct.readCharNoHang(eofErrorP, eofValue);
	}

	@Override
	public ReadCharResult readByte(final boolean eofErrorP, final LispStruct eofValue) {
		return inputStreamStruct.readByte(eofErrorP, eofValue);
	}

	@Override
	public Integer unreadChar(final Integer codePoint) {
		return inputStreamStruct.unreadChar(codePoint);
	}

	/*
	LISP-STRUCT
	 */

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
}
