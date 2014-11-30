/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.characters.CharacterConstants;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;

import java.math.BigInteger;

/**
 * Implements the illegal '#??" Lisp reader macros.
 */
public final class SharpIllegalReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * Singleton instance variable.
	 */
	public static final SharpIllegalReaderMacroFunction INSTANCE = new SharpIllegalReaderMacroFunction();

	/**
	 * Private constructor.
	 */
	private SharpIllegalReaderMacroFunction() {
		ReaderVariables.READTABLE.getValue().setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.RIGHT_PARENTHESIS, INSTANCE);
		ReaderVariables.READTABLE.getValue().setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LESS_THAN_SIGN, INSTANCE);

		ReaderVariables.READTABLE.getValue().setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.TAB, INSTANCE);
		ReaderVariables.READTABLE.getValue().setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.NEWLINE, INSTANCE);
		ReaderVariables.READTABLE.getValue().setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LINE_FEED, INSTANCE);
		ReaderVariables.READTABLE.getValue().setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.SPACE, INSTANCE);
		ReaderVariables.READTABLE.getValue().setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.PAGE, INSTANCE);
		ReaderVariables.READTABLE.getValue().setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.RETURN, INSTANCE);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		throw new ReaderErrorException("Illegal sharp character " + codePoint);
	}
}
