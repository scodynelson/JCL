/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import java.math.BigInteger;
import java.util.Optional;

import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.readtable.ReadtableCase;
import jcl.lang.statics.ReaderVariables;
import jcl.util.CodePointConstants;
import org.springframework.stereotype.Component;

/**
 * Implements the '$...' Lisp reader macro.
 */
@Component
public class DollarSignReaderMacroFunction extends ReaderMacroFunctionImpl {

	public DollarSignReaderMacroFunction() {
		super("DOLLAR-SIGN");
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
		ReaderVariables.READTABLE.getVariableValue().setMacroCharacter(CodePointConstants.DOLLAR_SIGN, this, false);
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint, final Optional<BigInteger> numberArgument) {
		assert codePoint == CodePointConstants.DOLLAR_SIGN;

		final ReadtableStruct readtable = ReaderVariables.READTABLE.getVariableValue();
		final ReadtableCase previousCase = readtable.getReadtableCase();

		readtable.setReadtableCase(ReadtableCase.PRESERVE);

		final String tokenString;
		try {
			final ExtendedTokenReaderMacroFunction.ReadExtendedToken extendedToken = ExtendedTokenReaderMacroFunction.readExtendedToken(inputStreamStruct, false);
			tokenString = extendedToken.getTokenString();
		} finally {
			readtable.setReadtableCase(previousCase);
		}

		if (ReaderVariables.READ_SUPPRESS.getVariableValue().booleanValue()) {
			return null;
		}

		return LispStructFactory.toJavaName(tokenString);
	}
}
