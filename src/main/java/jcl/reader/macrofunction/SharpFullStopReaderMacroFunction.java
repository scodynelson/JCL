/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.reader.CharacterConstants;
import jcl.reader.Reader;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.symbols.variables.Variable;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.math.BigInteger;

/**
 * Implements the '#.' Lisp reader macro.
 */
public final class SharpFullStopReaderMacroFunction extends ReaderMacroFunction {

	/**
	 * Singleton instance variable.
	 */
	public static final SharpFullStopReaderMacroFunction INSTANCE = new SharpFullStopReaderMacroFunction();

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(SharpFullStopReaderMacroFunction.class);

	/**
	 * Private constructor.
	 */
	private SharpFullStopReaderMacroFunction() {
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.FULL_STOP;

		final LispStruct lispToken = reader.read();
		if (Variable.READ_SUPPRESS.getValue().booleanValue()) {
			if (LOGGER.isDebugEnabled()) {
				LOGGER.debug("{} suppressed.", lispToken.printStruct());
			}
			return null;
		}

		if (!Variable.READ_EVAL.getValue().booleanValue()) {
			throw new ReaderErrorException("Attempt to read #. while *READ-EVAL* is bound to NIL.");
		}

		// TODO: need to evaluate and return the evaluated result
		// Evaluate the lisp token

		return lispToken;
	}
}