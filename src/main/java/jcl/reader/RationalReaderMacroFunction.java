/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader;

import jcl.LispStruct;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.numbers.IntegerStruct;
import jcl.numbers.RationalStruct;
import org.apache.commons.lang3.Range;

import java.math.BigInteger;

/**
 * Reader Macro Function for handling the reading of {@link RationalStruct}s, following proper radix rules for a
 * provided radix value.
 */
abstract class RationalReaderMacroFunction extends ExtendedTokenReaderMacroFunction {

	/**
	 * The valid range of radix values.
	 */
	@SuppressWarnings("MagicNumber")
	private static final Range<BigInteger> RADIX_RANGE = Range.between(BigInteger.valueOf(2), BigInteger.valueOf(36));

	/**
	 * Protected constructor that calls the {@link ExtendedTokenReaderMacroFunction} constructor, passing {@code false}
	 * for token escaping processing.
	 */
	RationalReaderMacroFunction() {
		super(false);
	}

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
	RationalStruct readRational(final Reader reader, final BigInteger radix) {
		if (ReaderVariables.READ_SUPPRESS.getValue().booleanValue()) {
			readExtendedToken(reader);
			return null;
		}

		if (radix == null) {
			throw new ReaderErrorException("Radix missing in #R.");
		}

		if (!RADIX_RANGE.contains(radix)) {
			throw new ReaderErrorException("Illegal radix for #R: " + radix + '.');
		}

		final IntegerStruct previousReadBase = ReaderVariables.READ_BASE.getValue();

		// alter the read-base
		ReaderVariables.READ_BASE.setValue(new IntegerStruct(radix));

		// read rational
		final LispStruct lispToken = reader.read();

		// reset the read-base
		ReaderVariables.READ_BASE.setValue(previousReadBase);

		if (lispToken instanceof RationalStruct) {
			return (RationalStruct) lispToken;
		}

		throw new ReaderErrorException("#R (base " + radix + ") value is not a rational: " + lispToken + '.');
	}
}
