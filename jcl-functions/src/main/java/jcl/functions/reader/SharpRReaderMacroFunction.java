/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import java.math.BigInteger;
import java.util.Optional;

import jcl.lang.LispStruct;
import jcl.lang.RationalStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.readtable.ReaderInputStreamStruct;
import jcl.lang.statics.ReaderVariables;
import jcl.util.CodePointConstants;
import org.apache.commons.lang3.Range;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.DependsOn;
import org.springframework.stereotype.Component;

/**
 * Implements the '#r' Lisp reader macro.
 */
@Component
@DependsOn("readerBootstrap")
public class SharpRReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * The valid range of radix values.
	 */
	@SuppressWarnings("MagicNumber")
	private static final Range<BigInteger> RADIX_RANGE = Range.between(BigInteger.valueOf(2), BigInteger.valueOf(36));

	/**
	 * {@link Autowired} {@link RationalReaderMacroFunction} used for reading {@link RationalStruct}s.
	 */
	private final RationalReaderMacroFunction rationalReaderMacroFunction;

	@Autowired
	public SharpRReaderMacroFunction(final RationalReaderMacroFunction rationalReaderMacroFunction) {
		super("SHARP-R");
		this.rationalReaderMacroFunction = rationalReaderMacroFunction;
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
		final ReadtableStruct readtable = ReaderVariables.READTABLE.getVariableValue();
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LATIN_SMALL_LETTER_R, this);
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LATIN_CAPITAL_LETTER_R, this);
	}

	@Override
	public LispStruct readMacro(final ReaderInputStreamStruct inputStreamStruct, final int codePoint, final Optional<BigInteger> numberArgument) {
		assert (codePoint == CodePointConstants.LATIN_SMALL_LETTER_R) || (codePoint == CodePointConstants.LATIN_CAPITAL_LETTER_R);

		if (!numberArgument.isPresent()) {
			throw new ReaderErrorException("Radix missing in #R.");
		}
		final BigInteger radix = numberArgument.get();

		if (!RADIX_RANGE.contains(radix)) {
			throw new ReaderErrorException("Illegal radix for #R: " + radix + '.');
		}

		return rationalReaderMacroFunction.readRational(inputStreamStruct, radix);
	}
}
