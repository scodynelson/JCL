/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.math.BigInteger;

import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.NILStruct;
import jcl.lang.RationalStruct;
import jcl.lang.readtable.Reader;
import jcl.lang.statics.ReaderVariables;
import org.springframework.stereotype.Component;

/**
 * Reader Macro Function for handling the reading of {@link RationalStruct}s, following proper radix rules for a
 * provided radix value.
 */
@Component
final class RationalReaderMacroFunction {

	/**
	 * Read in and returns a properly parsed {@link RationalStruct}, handling proper radix rules for reader number base
	 * handling based on the provided {@code radix} value.
	 *
	 * @param reader
	 * 		the {@link Reader} used to read in the {@link RationalStruct}
	 * @param radix
	 * 		the radix value used when reading numeric parts of the {@link RationalStruct}
	 *
	 * @return the properly parsed {@link RationalStruct}
	 */
	LispStruct readRational(final Reader reader, final BigInteger radix) {
		if (ReaderVariables.READ_SUPPRESS.getVariableValue().booleanValue()) {
			ExtendedTokenReaderMacroFunction.readExtendedToken(reader, false);
			return NILStruct.INSTANCE;
		}

		final IntegerStruct previousReadBase = ReaderVariables.READ_BASE.getVariableValue();

		// alter the read-base
		ReaderVariables.READ_BASE.setValue(LispStructFactory.toInteger(radix));

		// read rational
		final LispStruct token = reader.read(true, NILStruct.INSTANCE, true);

		// reset the read-base
		ReaderVariables.READ_BASE.setValue(previousReadBase);

		if (token instanceof RationalStruct) {
			return token;
		}

		throw new ReaderErrorException("#R (base " + radix + ") value is not a rational: " + token + '.');
	}
}
