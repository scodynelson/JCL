/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.math.BigInteger;
import java.util.Optional;
import javax.annotation.PostConstruct;

import jcl.lang.LispStruct;
import jcl.lang.java.JavaNameStruct;
import jcl.lang.readtable.Reader;
import jcl.lang.statics.ReaderVariables;
import jcl.lang.readtable.ReadtableCase;
import jcl.lang.readtable.ReadtableStruct;
import jcl.util.CodePointConstants;
import org.springframework.stereotype.Component;

/**
 * Implements the '$...' Lisp reader macro.
 */
@Component
public class DollarSignReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		ReaderVariables.READTABLE.getVariableValue().setMacroCharacter(CodePointConstants.DOLLAR_SIGN, this, false);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Optional<BigInteger> numberArgument) {
		assert codePoint == CodePointConstants.DOLLAR_SIGN;

		final ReadtableStruct readtable = ReaderVariables.READTABLE.getVariableValue();
		final ReadtableCase previousCase = readtable.getReadtableCase();

		readtable.setReadtableCase(ReadtableCase.PRESERVE);

		final String tokenString;
		try {
			final ExtendedTokenReaderMacroFunction.ReadExtendedToken extendedToken = ExtendedTokenReaderMacroFunction.readExtendedToken(reader, false);
			tokenString = extendedToken.getTokenString();
		} finally {
			readtable.setReadtableCase(previousCase);
		}

		if (ReaderVariables.READ_SUPPRESS.getVariableValue().booleanValue()) {
			return null;
		}

		return new JavaNameStruct(tokenString);
	}
}
