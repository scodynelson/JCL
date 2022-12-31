/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import jcl.lang.InputStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.util.CodePointConstants;

/**
 * Implements the '#-' Lisp reader macro.
 */
public final class SharpHyphenMinusReaderMacroFunction extends ReaderMacroFunctionImpl {

	public SharpHyphenMinusReaderMacroFunction() {
		super("SHARP-HYPHEN-MINUS");
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint,
	                            final IntegerStruct numberArgument) {
		assert codePoint == CodePointConstants.HYPHEN_MINUS;

		return FeaturesReaderMacroFunction.readFeatures(inputStreamStruct, true);
	}
}
