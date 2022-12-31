/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import jcl.lang.InputStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.statics.CommonLispSymbols;
import jcl.reader.Reader;
import jcl.util.CodePointConstants;

/**
 * Implements the '#x' Lisp reader macro.
 */
public final class SharpXReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * Radix value to use.
	 */
	private static final int RADIX = 16;

	public SharpXReaderMacroFunction() {
		super("SHARP-X");
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint,
	                            final IntegerStruct numberArgument) {
		assert (codePoint == CodePointConstants.LATIN_SMALL_LETTER_X) || (codePoint == CodePointConstants.LATIN_CAPITAL_LETTER_X);

		if (CommonLispSymbols.READ_SUPPRESS_VAR.getVariableValue().toJavaPBoolean()) {
			Reader.read(inputStreamStruct, true, NILStruct.INSTANCE, true);
			return NILStruct.INSTANCE;
		}

		return RationalReaderMacroFunction.readRational(inputStreamStruct, RADIX);
	}
}
