/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import jcl.lang.InputStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.util.CodePointConstants;

/**
 * Implements the '(...)' Lisp reader macro.
 */
public final class LeftParenthesisReaderMacroFunction extends ReaderMacroFunctionImpl {

	public LeftParenthesisReaderMacroFunction() {
		super("LEFT-PARENTHESIS");
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint,
	                            final IntegerStruct numberArgument) {
		assert codePoint == CodePointConstants.LEFT_PARENTHESIS;

		return ListReaderMacroFunction.readList(inputStreamStruct);
	}
}
