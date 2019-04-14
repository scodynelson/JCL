/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import java.math.BigInteger;
import java.util.Optional;

import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.util.CodePointConstants;
import org.apache.commons.lang3.Range;

/**
 * Implements the '#r' Lisp reader macro.
 */
public final class SharpRReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * The valid range of radix values.
	 */
	@SuppressWarnings("MagicNumber")
	private static final Range<BigInteger> RADIX_RANGE = Range.between(BigInteger.valueOf(2), BigInteger.valueOf(36));

	public SharpRReaderMacroFunction() {
		super("SHARP-R");
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint,
	                            final Optional<BigInteger> numberArgument) {
		assert (codePoint == CodePointConstants.LATIN_SMALL_LETTER_R) || (codePoint == CodePointConstants.LATIN_CAPITAL_LETTER_R);

		if (!numberArgument.isPresent()) {
			throw new ReaderErrorException("Radix missing in #R.");
		}
		final BigInteger radix = numberArgument.get();

		if (!RADIX_RANGE.contains(radix)) {
			throw new ReaderErrorException("Illegal radix for #R: " + radix + '.');
		}

		return RationalReaderMacroFunction.readRational(inputStreamStruct, radix);
	}
}
