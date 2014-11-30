/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.characters.CharacterConstants;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;

import java.math.BigInteger;

/**
 * Implements the '#-' Lisp reader macro.
 */
public final class SharpHyphenMinusReaderMacroFunction extends FeaturesReaderMacroFunction {

	/**
	 * Singleton instance variable.
	 */
	public static final SharpHyphenMinusReaderMacroFunction INSTANCE = new SharpHyphenMinusReaderMacroFunction();

	/**
	 * Private constructor.
	 */
	private SharpHyphenMinusReaderMacroFunction() {
		super(true);
		ReaderVariables.READTABLE.getValue().setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.HYPHEN_MINUS, INSTANCE);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.HYPHEN_MINUS;

		readFeatures(reader);
		return null;
	}
}
