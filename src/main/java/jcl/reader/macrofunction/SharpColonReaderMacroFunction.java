/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.characters.CharacterConstants;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
import jcl.symbols.SymbolStruct;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.math.BigInteger;

/**
 * Implements the '#:' Lisp reader macro.
 */
@Component
public class SharpColonReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(SharpColonReaderMacroFunction.class);

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		ReaderVariables.READTABLE.getValue().setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.COLON, this);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.COLON;

		final ExtendedTokenReaderMacroFunction.ReadExtendedToken readExtendedToken = ExtendedTokenReaderMacroFunction.readExtendedToken(reader, false);
		final String token = readExtendedToken.getToken();

		if (ReaderVariables.READ_SUPPRESS.getValue().booleanValue()) {
			if (LOGGER.isDebugEnabled()) {
				LOGGER.debug("{} suppressed.", token);
			}
			return null;
		}

		if (readExtendedToken.isHasPackageDelimiter()) {
			throw new ReaderErrorException("Symbol following #: contains a package marker: " + token);
		}
		return new SymbolStruct<>(token);
	}
}
