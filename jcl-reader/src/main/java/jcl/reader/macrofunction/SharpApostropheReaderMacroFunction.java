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
import jcl.reader.Reader;
import jcl.lang.readtable.ReaderInputStreamStruct;
import jcl.lang.statics.ReaderVariables;
import jcl.util.CodePointConstants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.DependsOn;
import org.springframework.stereotype.Component;

/**
 * Implements the '#'' Lisp reader macro.
 */
@Component
@DependsOn("readerBootstrap")
public class SharpApostropheReaderMacroFunction extends ReaderMacroFunctionImpl {

	private final Reader reader;

	@Autowired
	public SharpApostropheReaderMacroFunction(final Reader reader) {
		this.reader = reader;
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
		ReaderVariables.READTABLE.getVariableValue().setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.APOSTROPHE, this);
	}

	@Override
	public LispStruct readMacro(final ReaderInputStreamStruct inputStreamStruct, final int codePoint, final Optional<BigInteger> numberArgument) {
		assert codePoint == CodePointConstants.APOSTROPHE;

		final LispStruct token = reader.read(inputStreamStruct, true, NILStruct.INSTANCE, true);
		if (token == null) {
			throw new ReaderErrorException("Missing expression.");
		}

		return LispStructFactory.toProperList(SpecialOperatorStructImpl.FUNCTION, token);
	}
}
