/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import java.math.BigInteger;
import java.util.Optional;

import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.RationalStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.statics.ReaderVariables;
import jcl.util.CodePointConstants;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.DependsOn;
import org.springframework.stereotype.Component;

/**
 * Implements the '#x' Lisp reader macro.
 */
@Component
@DependsOn("readerBootstrap")
public class SharpXReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * Radix value to use.
	 */
	private static final int RADIX = 16;

	/**
	 * {@link Autowired} {@link RationalReaderMacroFunction} used for reading {@link RationalStruct}s.
	 */
	private final RationalReaderMacroFunction rationalReaderMacroFunction;

	public SharpXReaderMacroFunction(final RationalReaderMacroFunction rationalReaderMacroFunction) {
		super("SHARP-X");
		this.rationalReaderMacroFunction = rationalReaderMacroFunction;
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
		final ReadtableStruct readtable = ReaderVariables.READTABLE.getVariableValue();
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LATIN_SMALL_LETTER_X, this);
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LATIN_CAPITAL_LETTER_X, this);
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint, final Optional<BigInteger> numberArgument) {
		assert (codePoint == CodePointConstants.LATIN_SMALL_LETTER_X) || (codePoint == CodePointConstants.LATIN_CAPITAL_LETTER_X);

		return rationalReaderMacroFunction.readRational(inputStreamStruct, BigInteger.valueOf(RADIX));
	}
}
