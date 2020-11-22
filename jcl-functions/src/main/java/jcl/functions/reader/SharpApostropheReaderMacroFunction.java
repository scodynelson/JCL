/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import jcl.lang.InputStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.internal.SpecialOperatorStructImpl;
import jcl.reader.Reader;
import jcl.util.CodePointConstants;

/**
 * Implements the '#'' Lisp reader macro.
 */
public final class SharpApostropheReaderMacroFunction extends ReaderMacroFunctionImpl {

	public SharpApostropheReaderMacroFunction() {
		super("SHARP-APOSTROPHE");
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint,
	                            final IntegerStruct numberArgument) {
		assert codePoint == CodePointConstants.APOSTROPHE;

		final LispStruct token = Reader.read(inputStreamStruct, true, NILStruct.INSTANCE, true);
		if (token == null) {
			throw new ReaderErrorException("Missing expression.");
		}

		return ListStruct.toLispList(SpecialOperatorStructImpl.FUNCTION, token);
	}
}
