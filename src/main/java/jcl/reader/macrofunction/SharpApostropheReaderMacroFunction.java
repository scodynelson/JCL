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
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigInteger;

/**
 * Implements the '#'' Lisp reader macro.
 */
public final class SharpApostropheReaderMacroFunction extends ReaderMacroFunction {

	/**
	 * Singleton instance variable.
	 */
	public static final SharpApostropheReaderMacroFunction INSTANCE = new SharpApostropheReaderMacroFunction();

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(SharpApostropheReaderMacroFunction.class);

	/**
	 * Private constructor.
	 */
	private SharpApostropheReaderMacroFunction() {
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.APOSTROPHE;

		final LispStruct lispToken = reader.read();
		if (ReaderVariables.READ_SUPPRESS.getValue().booleanValue()) {
			if (LOGGER.isDebugEnabled()) {
				LOGGER.debug("{} suppressed.", lispToken.printStruct());
			}
			return null;
		}

		if (lispToken == null) {
			throw new ReaderErrorException("Missing expression.");
		}

		return ListStruct.buildProperList(SpecialOperator.FUNCTION, lispToken);
	}
}
