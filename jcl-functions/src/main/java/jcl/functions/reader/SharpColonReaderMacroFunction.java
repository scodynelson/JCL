/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import java.math.BigInteger;
import java.util.Optional;

import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.statics.ReaderVariables;
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
	                            final Optional<BigInteger> numberArgument) {
		assert codePoint == CodePointConstants.COLON;

		final ExtendedTokenReaderMacroFunction.ReadExtendedToken extendedToken
				= ExtendedTokenReaderMacroFunction.readExtendedToken(inputStreamStruct, false);
		final String tokenString = extendedToken.getTokenString();

		if (ReaderVariables.READ_SUPPRESS.getVariableValue().toJavaPBoolean()) {
			return NILStruct.INSTANCE;
		}

		if (extendedToken.isHasPackageDelimiter()) {
			throw new ReaderErrorException("Symbol following #: contains a package marker: " + tokenString);
		}
		return SymbolStruct.toLispSymbol(tokenString);
	}
}
