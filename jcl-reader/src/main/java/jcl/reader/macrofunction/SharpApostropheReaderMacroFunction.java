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
import jcl.lang.internal.SpecialOperatorStructImpl;
import jcl.lang.readtable.Reader;
import jcl.lang.statics.ReaderVariables;
import jcl.util.CodePointConstants;
import org.springframework.stereotype.Component;

/**
 * Implements the '#'' Lisp reader macro.
 */
@Component
public class SharpApostropheReaderMacroFunction extends ReaderMacroFunctionImpl {

	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
		ReaderVariables.READTABLE.getVariableValue().setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.APOSTROPHE, this);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Optional<BigInteger> numberArgument) {
		assert codePoint == CodePointConstants.APOSTROPHE;

		final LispStruct token = reader.read(true, NILStruct.INSTANCE, true);
		if (token == null) {
			throw new ReaderErrorException("Missing expression.");
		}

		return LispStructFactory.toProperList(SpecialOperatorStructImpl.FUNCTION, token);
	}
}
