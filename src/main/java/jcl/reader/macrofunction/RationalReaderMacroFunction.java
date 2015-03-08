/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.lists.NullStruct;
import jcl.numbers.IntegerStruct;
import jcl.numbers.RationalStruct;
import jcl.printer.Printer;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
import org.apache.commons.lang3.Range;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.Serializable;
import java.math.BigInteger;

/**
 * Reader Macro Function for handling the reading of {@link RationalStruct}s, following proper radix rules for a
 * provided radix value.
 */
@Component
final class RationalReaderMacroFunction implements Serializable {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -8849349790791106477L;

	/**
	 * The valid range of radix values.
	 */
	@SuppressWarnings("MagicNumber")
	private static final Range<BigInteger> RADIX_RANGE = Range.between(BigInteger.valueOf(2), BigInteger.valueOf(36));

	/**
	 * {@link Autowired} {@link Printer} used for printing elements and structures to the output stream.
	 */
	@Autowired
	private Printer printer;

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
		if (ReaderVariables.READ_SUPPRESS.getValue().booleanValue()) {
			ExtendedTokenReaderMacroFunction.readExtendedToken(reader, false);
			return NullStruct.INSTANCE;
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
		final LispStruct lispToken = reader.read(true, NullStruct.INSTANCE, true);

		// reset the read-base
		ReaderVariables.READ_BASE.setValue(previousReadBase);

		if (lispToken instanceof RationalStruct) {
			return lispToken;
		}

		final String printedLispStruct = printer.print(lispToken);
		throw new ReaderErrorException("#R (base " + radix + ") value is not a rational: " + printedLispStruct + '.');
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
