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
		init();
	}

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	private void init() {
		ReaderVariables.READTABLE.getValue().setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.HYPHEN_MINUS, this);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.HYPHEN_MINUS;

		readFeatures(reader);
		return null;
	}
}
