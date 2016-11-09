/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import java.math.BigInteger;
import java.util.Optional;

import jcl.lang.ConsStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.reader.Reader;
import jcl.lang.statics.ReaderVariables;
import jcl.lang.stream.ReadPeekResult;
import jcl.reader.ReaderContext;
import jcl.reader.ReaderContextHolder;
import jcl.util.CodePointConstants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Implements the ',' Lisp reader macro.
 */
@Component
public class CommaReaderMacroFunction extends ReaderMacroFunctionImpl {

	private final Reader reader;

	@Autowired
	public CommaReaderMacroFunction(final Reader reader) {
		super("COMMA");
		this.reader = reader;
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
		ReaderVariables.READTABLE.getVariableValue().setMacroCharacter(CodePointConstants.COMMA, this, false);
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint, final Optional<BigInteger> numberArgument) {
		assert codePoint == CodePointConstants.GRAVE_ACCENT;

		final ReaderContext context = ReaderContextHolder.getContext();
		final int currentBackquoteLevel = context.getBackquoteLevel();
		if (currentBackquoteLevel <= 0) {
			if (ReaderVariables.READ_SUPPRESS.getVariableValue().booleanValue()) {
				return NILStruct.INSTANCE;
			}

			throw new ReaderErrorException("Comma not inside a backquote.");
		}

		final ReadPeekResult readResult = reader.readChar(inputStreamStruct, true, NILStruct.INSTANCE, false);
		final int nextCodePoint = readResult.getResult();

		context.decrementBackquoteLevel();
		try {
			final ConsStruct commaCons;

			if (nextCodePoint == CodePointConstants.AT_SIGN) {
				final LispStruct token = reader.read(inputStreamStruct, true, NILStruct.INSTANCE, true);
				commaCons = LispStructFactory.toCons(BackquoteReaderMacroFunction.BQ_AT_FLAG, token);
			} else if (nextCodePoint == CodePointConstants.FULL_STOP) {
				final LispStruct token = reader.read(inputStreamStruct, true, NILStruct.INSTANCE, true);
				commaCons = LispStructFactory.toCons(BackquoteReaderMacroFunction.BQ_DOT_FLAG, token);
			} else {
				reader.unreadChar(inputStreamStruct, nextCodePoint);
				final LispStruct token = reader.read(inputStreamStruct, true, NILStruct.INSTANCE, true);
				commaCons = LispStructFactory.toCons(BackquoteReaderMacroFunction.BQ_COMMA_FLAG, token);
			}
			return commaCons;
		} finally {
			context.incrementBackquoteLevel();
		}
	}
}
