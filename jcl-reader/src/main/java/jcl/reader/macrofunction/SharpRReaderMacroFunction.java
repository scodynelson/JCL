/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.math.BigInteger;
import java.util.Optional;
import javax.annotation.PostConstruct;

import jcl.lang.CharacterConstants;
import jcl.lang.LispStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.function.ReaderMacroFunction;
import jcl.lang.number.RationalStruct;
import jcl.lang.readtable.Reader;
import jcl.lang.readtable.ReaderVariables;
import jcl.lang.readtable.ReadtableStruct;
import org.apache.commons.lang3.Range;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Implements the '#r' Lisp reader macro.
 */
@Component
public class SharpRReaderMacroFunction extends ReaderMacroFunction {

	/**
	 * The valid range of radix values.
	 */
	@SuppressWarnings("MagicNumber")
	private static final Range<BigInteger> RADIX_RANGE = Range.between(BigInteger.valueOf(2), BigInteger.valueOf(36));

	/**
	 * {@link Autowired} {@link RationalReaderMacroFunction} used for reading {@link RationalStruct}s.
	 */
	@Autowired
	private RationalReaderMacroFunction rationalReaderMacroFunction;

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		final ReadtableStruct readtable = ReaderVariables.READTABLE.getVariableValue();
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_SMALL_LETTER_R, this);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_CAPITAL_LETTER_R, this);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Optional<BigInteger> numberArgument) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_R) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_R);

		if (!numberArgument.isPresent()) {
			throw new ReaderErrorException("Radix missing in #R.");
		}
		final BigInteger radix = numberArgument.get();

		if (!RADIX_RANGE.contains(radix)) {
			throw new ReaderErrorException("Illegal radix for #R: " + radix + '.');
		}

		return rationalReaderMacroFunction.readRational(reader, radix);
	}
}