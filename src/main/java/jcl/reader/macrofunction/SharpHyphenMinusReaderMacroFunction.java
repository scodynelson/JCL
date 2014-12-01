/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.characters.CharacterConstants;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.math.BigInteger;

/**
 * Implements the '#-' Lisp reader macro.
 */
@Component
public class SharpHyphenMinusReaderMacroFunction extends FeaturesReaderMacroFunction {

	/**
	 * Private constructor.
	 */
	public SharpHyphenMinusReaderMacroFunction() {
		super(true);
	}

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
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
