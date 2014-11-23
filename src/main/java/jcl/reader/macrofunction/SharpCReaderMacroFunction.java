/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.reader.CharacterConstants;
import jcl.reader.Reader;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.lists.ListStruct;
import jcl.structs.numbers.ComplexStruct;
import jcl.structs.numbers.RealStruct;
import jcl.structs.symbols.variables.Variable;

import java.math.BigInteger;
import java.util.List;

/**
 * Implements the '#c' Lisp reader macro.
 */
public final class SharpCReaderMacroFunction extends ReaderMacroFunction {

	/**
	 * Singleton instance variable.
	 */
	public static final SharpCReaderMacroFunction INSTANCE = new SharpCReaderMacroFunction();

	/**
	 * Private constructor.
	 */
	private SharpCReaderMacroFunction() {
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_C) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_C);

		final LispStruct lispToken = reader.read();
		if (Variable.READ_SUPPRESS.getValue().booleanValue()) {
			return null;
		}

		if (!(lispToken instanceof ListStruct)) {
			throw new ReaderErrorException("Illegal complex number format: #C" + lispToken);
		}

		final ListStruct listToken = (ListStruct) lispToken;
		final List<LispStruct> lispTokens = listToken.getAsJavaList();
		if (lispTokens.size() != 2) {
			throw new ReaderErrorException("Illegal complex number format: #C" + lispToken);
		}

		final LispStruct real = lispTokens.get(0);
		if (!(real instanceof RealStruct)) {
			throw new ReaderErrorException("Only real numbers are valid tokens for #c. Got: " + real);
		}

		final LispStruct imaginary = lispTokens.get(1);
		if (!(imaginary instanceof RealStruct)) {
			throw new ReaderErrorException("Only real numbers are valid tokens for #c. Got: " + imaginary);
		}
		return new ComplexStruct((RealStruct) real, (RealStruct) imaginary);
	}
}
