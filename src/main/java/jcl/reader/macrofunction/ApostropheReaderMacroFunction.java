/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.characters.CharacterConstants;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.lists.ListStruct;
import jcl.reader.Reader;
import jcl.reader.ReaderVariables;
import jcl.symbols.SpecialOperator;

import java.math.BigInteger;

/**
 * Implements the ''' Lisp reader macro.
 */
public final class ApostropheReaderMacroFunction extends ReaderMacroFunction {

	/**
	 * Singleton instance variable.
	 */
	public static final ApostropheReaderMacroFunction INSTANCE = new ApostropheReaderMacroFunction();

	/**
	 * Private constructor.
	 */
	private ApostropheReaderMacroFunction() {
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.APOSTROPHE;

		final LispStruct expression = reader.read();
		if (ReaderVariables.READ_SUPPRESS.getValue().booleanValue()) {
			return null;
		}

		if (expression == null) {
			throw new ReaderErrorException("Missing expression.");
		}

		return ListStruct.buildProperList(SpecialOperator.QUOTE, expression);
	}
}
