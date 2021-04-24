/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import jcl.lang.InputStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.PathnameStruct;
import jcl.lang.StringStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.statics.CommonLispSymbols;
import jcl.reader.Reader;
import jcl.util.CodePointConstants;

/**
 * Implements the '#p' Lisp reader macro.
 */
public final class SharpPReaderMacroFunction extends ReaderMacroFunctionImpl {

	public SharpPReaderMacroFunction() {
		super("SHARP-P");
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint,
	                            final IntegerStruct numberArgument) {
		assert (codePoint == CodePointConstants.LATIN_SMALL_LETTER_P) || (codePoint == CodePointConstants.LATIN_CAPITAL_LETTER_P);

		final LispStruct token = Reader.read(inputStreamStruct, true, NILStruct.INSTANCE, true);
		if (CommonLispSymbols.READ_SUPPRESS_VAR.getVariableValue().toJavaPBoolean()) {
			return NILStruct.INSTANCE;
		}

		if (token instanceof StringStruct) {
			final StringStruct pathnameString = (StringStruct) token;
			final String namestring = pathnameString.toJavaString();
			return PathnameStruct.toPathname(namestring);
		} else {
			throw new ReaderErrorException("The value " + token + " is not of expected type STRING in argument to #P.");
		}
	}
}
