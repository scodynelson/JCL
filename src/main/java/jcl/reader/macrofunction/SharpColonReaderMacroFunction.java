/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.math.BigInteger;
import java.util.Optional;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.characters.CharacterConstants;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.lists.NullStruct;
import jcl.reader.Reader;
import jcl.reader.ReaderMacroFunction;
import jcl.reader.struct.ReaderVariables;
import jcl.symbols.SymbolStruct;
import org.springframework.stereotype.Component;

/**
 * Implements the '#:' Lisp reader macro.
 */
@Component
public class SharpColonReaderMacroFunction extends ReaderMacroFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -7264077548468382838L;

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		ReaderVariables.READTABLE.getVariableValue().setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.COLON, this);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Optional<BigInteger> numberArgument) {
		assert codePoint == CharacterConstants.COLON;

		final ExtendedTokenReaderMacroFunction.ReadExtendedToken extendedToken = ExtendedTokenReaderMacroFunction.readExtendedToken(reader, false);
		final String tokenString = extendedToken.getTokenString();

		if (ReaderVariables.READ_SUPPRESS.getVariableValue().booleanValue()) {
			return NullStruct.INSTANCE;
		}

		if (extendedToken.isHasPackageDelimiter()) {
			throw new ReaderErrorException("Symbol following #: contains a package marker: " + tokenString);
		}
		return new SymbolStruct(tokenString);
	}
}
