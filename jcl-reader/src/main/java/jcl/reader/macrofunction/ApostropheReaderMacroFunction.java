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
import org.springframework.stereotype.Component;

/**
 * Implements the ''' Lisp reader macro.
 */
@Component
public class ApostropheReaderMacroFunction extends ReaderMacroFunctionImpl {

	private final Reader reader;

	@Autowired
	public ApostropheReaderMacroFunction(final Reader reader) {
		this.reader = reader;
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
		ReaderVariables.READTABLE.getVariableValue().setMacroCharacter(CodePointConstants.APOSTROPHE, this, false);
	}

	@Override
	public LispStruct readMacro(final ReaderInputStreamStruct inputStreamStruct, final int codePoint, final Optional<BigInteger> numberArgument) {
		assert codePoint == CodePointConstants.APOSTROPHE;

		final LispStruct token = reader.read(inputStreamStruct, true, NILStruct.INSTANCE, true);
		if (ReaderVariables.READ_SUPPRESS.getVariableValue().booleanValue()) {
			return NILStruct.INSTANCE;
		}

		if (token == null) {
			throw new ReaderErrorException("Missing expression.");
		}

		return LispStructFactory.toProperList(SpecialOperatorStructImpl.QUOTE, token);
	}
}
