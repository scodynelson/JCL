/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import jcl.lang.InputStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ReadtableCase;
import jcl.lang.ReadtableStruct;
import jcl.lang.java.JavaClassStruct;
import jcl.lang.java.JavaObjectStruct;
import jcl.lang.statics.CommonLispSymbols;
import jcl.util.CodePointConstants;

/**
 * Implements the '@...' Lisp reader macro.
 */
public final class AtSignReaderMacroFunction extends ReaderMacroFunctionImpl {

	public AtSignReaderMacroFunction() {
		super("AT-SIGN");
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint,
	                            final IntegerStruct numberArgument) {
		assert codePoint == CodePointConstants.AT_SIGN;

		final ReadtableStruct readtable = CommonLispSymbols.READTABLE_VAR.getVariableValue();
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

		if (CommonLispSymbols.READ_SUPPRESS_VAR.getVariableValue().toJavaPBoolean()) {
			return null;
		}

		final JavaClassStruct javaClass = JavaClassStruct.toJavaClass(tokenString);
		return JavaObjectStruct.toJavaObject(javaClass.getJavaClass());
	}
}
