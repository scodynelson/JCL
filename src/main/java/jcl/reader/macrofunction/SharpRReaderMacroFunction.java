/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.characters.CharacterConstants;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
import jcl.reader.struct.ReadtableStruct;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.math.BigInteger;

/**
 * Implements the '#r' Lisp reader macro.
 */
@Component
public class SharpRReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		final ReadtableStruct readtable = ReaderVariables.READTABLE.getValue();
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_SMALL_LETTER_R, this);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_CAPITAL_LETTER_R, this);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_R) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_R);

		return RationalReaderMacroFunction.readRational(reader, numArg);
	}
}