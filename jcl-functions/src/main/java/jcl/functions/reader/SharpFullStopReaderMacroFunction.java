/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import jcl.compiler.function.InternalEval;
import jcl.lang.InputStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.statics.ReaderVariables;
import jcl.reader.Reader;
import jcl.util.CodePointConstants;

/**
 * Implements the '#.' Lisp reader macro.
 */
public final class SharpFullStopReaderMacroFunction extends ReaderMacroFunctionImpl {

	public SharpFullStopReaderMacroFunction() {
		super("SHARP-FULL-STOP");
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint,
	                            final IntegerStruct numberArgument) {
		assert codePoint == CodePointConstants.FULL_STOP;

		final LispStruct token = Reader.read(inputStreamStruct, true, NILStruct.INSTANCE, true);
		if (ReaderVariables.READ_SUPPRESS.getVariableValue().toJavaPBoolean()) {
			return NILStruct.INSTANCE;
		}

		if (!ReaderVariables.READ_EVAL.getVariableValue().toJavaPBoolean()) {
			throw new ReaderErrorException("Attempt to read #. while *READ-EVAL* is bound to NIL.");
		}

		return InternalEval.eval(token);
	}
}
