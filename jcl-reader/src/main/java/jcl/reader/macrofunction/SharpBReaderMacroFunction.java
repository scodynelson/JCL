/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.math.BigInteger;
import java.util.Optional;
import javax.annotation.PostConstruct;

import jcl.lang.character.CharacterConstants;
import jcl.lang.LispStruct;
import jcl.lang.function.ReaderMacroFunction;
import jcl.lang.number.RationalStruct;
import jcl.lang.readtable.Reader;
import jcl.lang.readtable.ReaderVariables;
import jcl.lang.readtable.ReadtableStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Implements the '#b' Lisp reader macro.
 */
@Component
public class SharpBReaderMacroFunction extends ReaderMacroFunction {

	/**
	 * Radix value to use.
	 */
	private static final int RADIX = 2;

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
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_SMALL_LETTER_B, this);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_CAPITAL_LETTER_B, this);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Optional<BigInteger> numberArgument) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_B) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_B);

		return rationalReaderMacroFunction.readRational(reader, BigInteger.valueOf(RADIX));
	}
}
