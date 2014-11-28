/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.characters.CharacterConstants;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.reader.Reader;
import jcl.reader.ReaderVariables;

import java.math.BigInteger;
import java.util.UUID;

/**
 * Implements the '#=' Lisp reader macro.
 */
public final class SharpEqualsSignReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * Singleton instance variable.
	 */
	public static final SharpEqualsSignReaderMacroFunction INSTANCE = new SharpEqualsSignReaderMacroFunction();

	/**
	 * Private constructor.
	 */
	private SharpEqualsSignReaderMacroFunction() {
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.EQUALS_SIGN;

		if (ReaderVariables.READ_SUPPRESS.getValue().booleanValue()) {
			return null;
		}

		if (numArg == null) {
			throw new ReaderErrorException("Missing label for #=.");
		}

		if (SharpTagReaderConstants.SHARP_EQUAL_FINAL_TABLE.containsKey(numArg)
				|| SharpTagReaderConstants.SHARP_EQUAL_TEMP_TABLE.containsKey(numArg)) {
			throw new ReaderErrorException("Label already defined: #" + numArg + '=');
		}

		final UUID tag = UUID.randomUUID();

		SharpTagReaderConstants.SHARP_EQUAL_TEMP_TABLE.put(numArg, tag);
		final LispStruct token = reader.read();
		SharpTagReaderConstants.SHARP_EQUAL_REPL_TABLE.put(tag, token);

		SharpTagReaderConstants.SHARP_EQUAL_FINAL_TABLE.put(numArg, token);

		return null;
	}
}
