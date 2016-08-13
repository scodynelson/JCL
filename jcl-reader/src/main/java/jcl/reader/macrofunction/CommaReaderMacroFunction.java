/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.math.BigInteger;
import java.util.Optional;

import jcl.lang.ConsStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.readtable.Reader;
import jcl.lang.statics.ReaderVariables;
import jcl.lang.stream.ReadPeekResult;
import jcl.util.CodePointConstants;
import org.springframework.stereotype.Component;

/**
 * Implements the ',' Lisp reader macro.
 */
@Component
public class CommaReaderMacroFunction extends ReaderMacroFunctionImpl {

	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
		ReaderVariables.READTABLE.getVariableValue().setMacroCharacter(CodePointConstants.COMMA, this, false);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Optional<BigInteger> numberArgument) {
		assert codePoint == CodePointConstants.GRAVE_ACCENT;

		final int currentBackquoteLevel = reader.getBackquoteLevel();
		if (currentBackquoteLevel <= 0) {
			if (ReaderVariables.READ_SUPPRESS.getVariableValue().booleanValue()) {
				return NILStruct.INSTANCE;
			}

			throw new ReaderErrorException("Comma not inside a backquote.");
		}

		final ReadPeekResult readResult = reader.readChar(true, NILStruct.INSTANCE, false);
		final int nextCodePoint = readResult.getResult();

		reader.decrementBackquoteLevel();
		try {
			final ConsStruct commaCons;

			if (nextCodePoint == CodePointConstants.AT_SIGN) {
				final LispStruct token = reader.read(true, NILStruct.INSTANCE, true);
				commaCons = LispStructFactory.toCons(BackquoteReaderMacroFunction.BQ_AT_FLAG, token);
			} else if (nextCodePoint == CodePointConstants.FULL_STOP) {
				final LispStruct token = reader.read(true, NILStruct.INSTANCE, true);
				commaCons = LispStructFactory.toCons(BackquoteReaderMacroFunction.BQ_DOT_FLAG, token);
			} else {
				reader.unreadChar(nextCodePoint);
				final LispStruct token = reader.read(true, NILStruct.INSTANCE, true);
				commaCons = LispStructFactory.toCons(BackquoteReaderMacroFunction.BQ_COMMA_FLAG, token);
			}
			return commaCons;
		} finally {
			reader.incrementBackquoteLevel();
		}
	}
}
