/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import jcl.characters.CharacterConstants;
import jcl.characters.CharacterStruct;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import javax.annotation.PostConstruct;
import java.math.BigInteger;

/**
 * Implements the '#\' Lisp reader macro.
 */
@Component
public class SharpBackslashReaderMacroFunction extends ReaderMacroFunctionImpl {

	/**
	 * The logger for this class.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(SharpBackslashReaderMacroFunction.class);

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		ReaderVariables.READTABLE.getValue().setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.BACKSLASH, this);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert codePoint == CharacterConstants.BACKSLASH;

		final ExtendedTokenReaderMacroFunction.ReadExtendedToken readExtendedToken = ExtendedTokenReaderMacroFunction.readExtendedToken(reader, true);
		final String token = readExtendedToken.getToken();

		if (ReaderVariables.READ_SUPPRESS.getValue().booleanValue()) {
			if (LOGGER.isDebugEnabled()) {
				LOGGER.debug("{} suppressed.", token);
			}
			return null;
		}

		final int maxCharTokenLength = 1;
		if (StringUtils.length(token) == maxCharTokenLength) {
			final char theChar = token.charAt(0);
			return new CharacterStruct(theChar);
		}

		Integer nameCodePoint = null;
		for (final CharacterName characterName : CharacterName.values()) {
			final String name = characterName.getName();
			if (StringUtils.equalsIgnoreCase(token, name)) {
				nameCodePoint = characterName.getCodePoint();
				break;
			}
		}

		if (nameCodePoint == null) {
			throw new ReaderErrorException("Unrecognized character name: " + token);
		}

		return new CharacterStruct(nameCodePoint);
	}
}
