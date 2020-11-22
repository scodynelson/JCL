/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import jcl.lang.InputStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.util.CodePointConstants;

/**
 * Implements the '#+' Lisp reader macro.
 */
public final class SharpPlusSignReaderMacroFunction extends ReaderMacroFunctionImpl {

	public SharpPlusSignReaderMacroFunction() {
		super("SHARP-PLUS-SIGN");
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint,
	                            final IntegerStruct numberArgument) {
		assert codePoint == CodePointConstants.PLUS_SIGN;

		FeaturesReaderMacroFunction.readFeatures(inputStreamStruct, false);
		return null;
	}
}
