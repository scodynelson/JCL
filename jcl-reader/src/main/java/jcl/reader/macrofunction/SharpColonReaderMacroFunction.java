/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.math.BigInteger;
import java.util.Optional;

import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.readtable.ReaderInputStreamStruct;
import jcl.lang.statics.ReaderVariables;
import jcl.util.CodePointConstants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.DependsOn;
import org.springframework.stereotype.Component;

/**
 * Implements the '#:' Lisp reader macro.
 */
@Component
@DependsOn("readerBootstrap")
public class SharpColonReaderMacroFunction extends ReaderMacroFunctionImpl {

	private final ExtendedTokenReaderMacroFunction extendedTokenReaderMacroFunction;

	@Autowired
	public SharpColonReaderMacroFunction(final ExtendedTokenReaderMacroFunction extendedTokenReaderMacroFunction) {
		this.extendedTokenReaderMacroFunction = extendedTokenReaderMacroFunction;
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
		ReaderVariables.READTABLE.getVariableValue().setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.COLON, this);
	}

	@Override
	public LispStruct readMacro(final ReaderInputStreamStruct inputStreamStruct, final int codePoint, final Optional<BigInteger> numberArgument) {
		assert codePoint == CodePointConstants.COLON;

		final ExtendedTokenReaderMacroFunction.ReadExtendedToken extendedToken = extendedTokenReaderMacroFunction.readExtendedToken(inputStreamStruct, false);
		final String tokenString = extendedToken.getTokenString();

		if (ReaderVariables.READ_SUPPRESS.getVariableValue().booleanValue()) {
			return NILStruct.INSTANCE;
		}

		if (extendedToken.isHasPackageDelimiter()) {
			throw new ReaderErrorException("Symbol following #: contains a package marker: " + tokenString);
		}
		return LispStructFactory.toSymbol(tokenString);
	}
}
