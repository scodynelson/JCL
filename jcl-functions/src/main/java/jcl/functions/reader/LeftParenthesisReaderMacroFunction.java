/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import java.math.BigInteger;
import java.util.Optional;

import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.statics.ReaderVariables;
import jcl.util.CodePointConstants;

/**
 * Implements the '(...)' Lisp reader macro.
 */
public final class LeftParenthesisReaderMacroFunction extends ReaderMacroFunctionImpl {

	public LeftParenthesisReaderMacroFunction() {
		super("LEFT-PARENTHESIS");
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
		ReaderVariables.READTABLE.getVariableValue().setMacroCharacter(CodePointConstants.LEFT_PARENTHESIS, this, false);
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint,
	                            final Optional<BigInteger> numberArgument) {
		assert codePoint == CodePointConstants.LEFT_PARENTHESIS;

		return ListReaderMacroFunction.readList(inputStreamStruct);
	}
}
