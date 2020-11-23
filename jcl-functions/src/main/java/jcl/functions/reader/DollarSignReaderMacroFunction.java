/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import jcl.lang.InputStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.java.JavaNameStruct;
import jcl.lang.ReadtableCase;
import jcl.lang.statics.ReaderVariables;
import jcl.util.CodePointConstants;

/**
 * Implements the '$...' Lisp reader macro.
 */
public final class DollarSignReaderMacroFunction extends ReaderMacroFunctionImpl {

	public DollarSignReaderMacroFunction() {
		super("DOLLAR-SIGN");
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint,
	                            final IntegerStruct numberArgument) {
		assert codePoint == CodePointConstants.DOLLAR_SIGN;

		final ReadtableStruct readtable = ReaderVariables.READTABLE.getVariableValue();
		final ReadtableCase previousCase = readtable.getReadtableCase();

		readtable.setReadtableCase(ReadtableCase.PRESERVE);

		final String tokenString;
		try {
			final ExtendedTokenReaderMacroFunction.ReadExtendedToken extendedToken
					= ExtendedTokenReaderMacroFunction.readExtendedToken(inputStreamStruct, false);
			tokenString = extendedToken.getTokenString();
		} finally {
			readtable.setReadtableCase(previousCase);
		}

		if (ReaderVariables.READ_SUPPRESS.getVariableValue().toJavaPBoolean()) {
			return null;
		}

		return JavaNameStruct.toJavaName(tokenString);
	}
}
