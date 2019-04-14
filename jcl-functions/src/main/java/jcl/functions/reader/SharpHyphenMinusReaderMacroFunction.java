/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import java.math.BigInteger;
import java.util.Optional;

import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.statics.ReaderVariables;
import jcl.util.CodePointConstants;

/**
 * Implements the '#-' Lisp reader macro.
 */
public final class SharpHyphenMinusReaderMacroFunction extends ReaderMacroFunctionImpl {

	public SharpHyphenMinusReaderMacroFunction() {
		super("SHARP-HYPHEN-MINUS");
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
		ReaderVariables.READTABLE.getVariableValue().setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.HYPHEN_MINUS, this);
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint,
	                            final Optional<BigInteger> numberArgument) {
		assert codePoint == CodePointConstants.HYPHEN_MINUS;

		FeaturesReaderMacroFunction.readFeatures(inputStreamStruct, true);
		return null;
	}
}
