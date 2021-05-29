/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import jcl.lang.InputStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.RationalStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.statics.CommonLispSymbols;
import jcl.reader.Reader;
import lombok.experimental.UtilityClass;

/**
 * Reader Macro Function for handling the reading of {@link RationalStruct}s, following proper radix rules for a
 * provided radix value.
 */
@UtilityClass
final class RationalReaderMacroFunction {

	/**
	 * Read in and returns a properly parsed {@link RationalStruct}, handling proper radix rules for reader number base
	 * handling based on the provided {@code radix} value.
	 *
	 * @param inputStreamStruct
	 * 		the {@link InputStreamStruct} to read in the {@link RationalStruct}
	 * @param radix
	 * 		the radix value used when reading numeric parts of the {@link RationalStruct}
	 *
	 * @return the properly parsed {@link RationalStruct}
	 */
	static LispStruct readRational(final InputStreamStruct inputStreamStruct, final int radix) {
		if (CommonLispSymbols.READ_SUPPRESS_VAR.getVariableValue().toJavaPBoolean()) {
			ExtendedTokenReaderMacroFunction.readExtendedToken(inputStreamStruct, false);
			return NILStruct.INSTANCE;
		}

		final IntegerStruct previousReadBase = CommonLispSymbols.READ_BASE_VAR.getVariableValue();

		// read rational
		final LispStruct token;
		try {
			// alter the read-base
			CommonLispSymbols.READ_BASE_VAR.setfSymbolValue(IntegerStruct.toLispInteger(radix));

			token = Reader.read(inputStreamStruct, true, NILStruct.INSTANCE, true);
		} finally {
			// reset the read-base
			CommonLispSymbols.READ_BASE_VAR.setfSymbolValue(previousReadBase);
		}

		if (token instanceof RationalStruct) {
			return token;
		}

		throw new ReaderErrorException("#R (base " + radix + ") value is not a rational: " + token + '.');
	}
}
