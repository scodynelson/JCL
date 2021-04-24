/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import jcl.lang.InputStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.statics.CommonLispSymbols;
import jcl.util.CodePointConstants;

/**
 * Implements the '#:' Lisp reader macro.
 */
public final class SharpColonReaderMacroFunction extends ReaderMacroFunctionImpl {

	public SharpColonReaderMacroFunction() {
		super("SHARP-COLON");
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint,
	                            final IntegerStruct numberArgument) {
		assert codePoint == CodePointConstants.COLON;

		final ExtendedTokenReaderMacroFunction.ReadExtendedToken extendedToken
				= ExtendedTokenReaderMacroFunction.readExtendedToken(inputStreamStruct, false);
		final String tokenString = extendedToken.getTokenString();

		if (CommonLispSymbols.READ_SUPPRESS_VAR.getVariableValue().toJavaPBoolean()) {
			return NILStruct.INSTANCE;
		}

		if (extendedToken.isHasPackageDelimiter()) {
			throw new ReaderErrorException("Symbol following #: contains a package marker: " + tokenString);
		}
		return SymbolStruct.toLispSymbol(tokenString);
	}
}
