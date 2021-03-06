/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import jcl.lang.InputStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.util.CodePointConstants;

/**
 * Implements the ')' Lisp reader macro.
 */
public final class RightParenthesisReaderMacroFunction extends ReaderMacroFunctionImpl {

	public RightParenthesisReaderMacroFunction() {
		super("RIGHT-PARENTHESIS");
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint,
	                            final IntegerStruct numberArgument) {
		assert codePoint == CodePointConstants.RIGHT_PARENTHESIS;

		throw new ReaderErrorException("Unmatched close parenthesis.");
	}
}
