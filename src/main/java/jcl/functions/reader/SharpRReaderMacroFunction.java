/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import jcl.lang.InputStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.statics.CommonLispSymbols;
import jcl.reader.Reader;
import jcl.util.CodePointConstants;

/**
 * Implements the '#r' Lisp reader macro.
 */
public final class SharpRReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * The lower bound of radix values.
	 */
	private static final int RADIX_LOWER_BOUND = 2;

	/**
	 * The upper bound of radix values.
	 */
	private static final int RADIX_UPPER_BOUND = 36;

	public SharpRReaderMacroFunction() {
		super("SHARP-R");
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint,
	                            final IntegerStruct numberArgument) {
		assert (codePoint == CodePointConstants.LATIN_SMALL_LETTER_R) || (codePoint == CodePointConstants.LATIN_CAPITAL_LETTER_R);

		if (CommonLispSymbols.READ_SUPPRESS_VAR.getVariableValue().toJavaPBoolean()) {
			Reader.read(inputStreamStruct, true, NILStruct.INSTANCE, true);
			return NILStruct.INSTANCE;
		}

		if (numberArgument == null) {
			throw new ReaderErrorException("Radix missing in #R.");
		}
		final int radix = numberArgument.toJavaInt();

		if ((radix >= RADIX_LOWER_BOUND) && (radix <= RADIX_UPPER_BOUND)) {
			throw new ReaderErrorException("Illegal radix for #R: " + radix + '.');
		}

		return RationalReaderMacroFunction.readRational(inputStreamStruct, radix);
	}
}
