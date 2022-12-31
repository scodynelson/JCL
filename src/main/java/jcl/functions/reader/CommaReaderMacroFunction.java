/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import jcl.lang.ConsStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.ReadCharResult;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.statics.CommonLispSymbols;
import jcl.reader.Reader;
import jcl.reader.ReaderContext;
import jcl.reader.ReaderContextHolder;
import jcl.util.CodePointConstants;

/**
 * Implements the ',' Lisp reader macro.
 */
public final class CommaReaderMacroFunction extends ReaderMacroFunctionImpl {

	public CommaReaderMacroFunction() {
		super("COMMA");
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint,
	                            final IntegerStruct numberArgument) {
		assert codePoint == CodePointConstants.COMMA;

		final ReaderContext context = ReaderContextHolder.getContext();
		final int currentBackquoteLevel = context.getBackquoteLevel();
		if (currentBackquoteLevel <= 0) {
			if (CommonLispSymbols.READ_SUPPRESS_VAR.getVariableValue().toJavaPBoolean()) {
				return NILStruct.INSTANCE;
			}

			throw new ReaderErrorException("Comma not inside a backquote.");
		}

		final ReadCharResult readResult = inputStreamStruct.readChar(true, NILStruct.INSTANCE);
		final int nextCodePoint = readResult.getResult();

		context.decrementBackquoteLevel();
		try {
			final ConsStruct commaCons;

			if (nextCodePoint == CodePointConstants.AT_SIGN) {
				final LispStruct token = Reader.read(inputStreamStruct, true, NILStruct.INSTANCE, true);
				commaCons = ConsStruct.toLispCons(BackquoteReaderMacroFunction.BQ_AT_FLAG, token);
			} else if (nextCodePoint == CodePointConstants.FULL_STOP) {
				final LispStruct token = Reader.read(inputStreamStruct, true, NILStruct.INSTANCE, true);
				commaCons = ConsStruct.toLispCons(BackquoteReaderMacroFunction.BQ_DOT_FLAG, token);
			} else {
				inputStreamStruct.unreadChar(nextCodePoint);
				final LispStruct token = Reader.read(inputStreamStruct, true, NILStruct.INSTANCE, true);
				commaCons = ConsStruct.toLispCons(BackquoteReaderMacroFunction.BQ_COMMA_FLAG, token);
			}

			if (CommonLispSymbols.READ_SUPPRESS_VAR.getVariableValue().toJavaPBoolean()) {
				return NILStruct.INSTANCE;
			}
			return commaCons;
		} finally {
			context.incrementBackquoteLevel();
		}
	}
}
