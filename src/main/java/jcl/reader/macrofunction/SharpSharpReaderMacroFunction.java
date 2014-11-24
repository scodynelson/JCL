/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.reader.CharacterConstants;
import jcl.reader.Reader;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.symbols.variables.Variable;

import java.math.BigInteger;
import java.util.UUID;

/**
 * Implements the '##' Lisp reader macro.
 */
public final class SharpSharpReaderMacroFunction extends ReaderMacroFunction {

	/**
	 * Singleton instance variable.
	 */
	public static final SharpSharpReaderMacroFunction INSTANCE = new SharpSharpReaderMacroFunction();

	/**
	 * Private constructor.
	 */
	private SharpSharpReaderMacroFunction() {
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.NUMBER_SIGN;

		if (Variable.READ_SUPPRESS.getValue().booleanValue()) {
			return null;
		}

		if (numArg == null) {
			throw new ReaderErrorException("Missing label for ##.");
		}

		final LispStruct labelObject = SharpTagReaderConstants.SHARP_EQUAL_FINAL_TABLE.get(numArg);
		if (labelObject != null) {
			return labelObject;
		}

		final UUID possibleLabelTag = SharpTagReaderConstants.SHARP_EQUAL_TEMP_TABLE.get(numArg);
		final LispStruct possibleLabelObject = SharpTagReaderConstants.SHARP_EQUAL_REPL_TABLE.get(possibleLabelTag);
		if (possibleLabelObject != null) {
			return possibleLabelObject;
		}

		throw new ReaderErrorException("Reference to undefined label #" + numArg + '#');
	}
}
