/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.reader.CharacterConstants;
import jcl.reader.Reader;
import jcl.structs.conditions.exceptions.ReaderErrorException;
import jcl.structs.symbols.SymbolStruct;
import jcl.structs.symbols.variables.Variable;

import java.math.BigInteger;

/**
 * Implements the '#:' Lisp reader macro.
 */
public final class SharpColonReaderMacroFunction extends ExtendedTokenReaderMacroFunction {

	/**
	 * Singleton instance variable.
	 */
	public static final SharpColonReaderMacroFunction INSTANCE = new SharpColonReaderMacroFunction();

	/**
	 * Private constructor.
	 */
	private SharpColonReaderMacroFunction() {
		super(false);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.COLON;

		final ReadExtendedToken readExtendedToken = readExtendedToken(reader);
		final String token = readExtendedToken.getToken();

		if (Variable.READ_SUPPRESS.getValue().booleanValue()) {
			return null;
		}

		if (readExtendedToken.hasPackageDelimiter()) {
			throw new ReaderErrorException("Symbol following #: contains a package marker: " + token);
		}
		return new SymbolStruct<>(token);
	}
}
