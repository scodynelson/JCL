/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.math.BigInteger;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.characters.CharacterConstants;
import jcl.characters.CharacterStruct;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.reader.Reader;
import jcl.reader.ReaderMacroFunction;
import jcl.reader.struct.ReaderVariables;
import jcl.types.Null;
import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

/**
 * Implements the '#\' Lisp reader macro.
 */
@Component
public class SharpBackslashReaderMacroFunction extends ReaderMacroFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 3860297910163190444L;

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		ReaderVariables.READTABLE.getValue().setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.BACKSLASH, this);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numberArgument) {
		assert codePoint == CharacterConstants.BACKSLASH;

		final ExtendedTokenReaderMacroFunction.ReadExtendedToken extendedToken = ExtendedTokenReaderMacroFunction.readExtendedToken(reader, true);
		final String tokenString = extendedToken.getTokenString();

		if (ReaderVariables.READ_SUPPRESS.getValue().booleanValue()) {
			return Null.INSTANCE;
		}

		final int maxTokenStringLength = 1;
		if (StringUtils.length(tokenString) == maxTokenStringLength) {
			final char characterToken = tokenString.charAt(0);
			return new CharacterStruct(characterToken);
		}

		Integer nameCodePoint = null;
		for (final CharacterName characterName : CharacterName.values()) {
			final String name = characterName.getName();
			if (StringUtils.equalsIgnoreCase(tokenString, name)) {
				nameCodePoint = characterName.getCodePoint();
				break;
			}
		}

		if (nameCodePoint == null) {
			throw new ReaderErrorException("Unrecognized character name: " + tokenString);
		}

		return new CharacterStruct(nameCodePoint);
	}
}
