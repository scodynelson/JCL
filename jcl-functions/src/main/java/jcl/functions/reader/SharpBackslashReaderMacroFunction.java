/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import java.lang.*;
import java.math.BigInteger;
import java.util.Optional;

import com.ibm.icu.lang.UCharacter;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.readtable.ReaderInputStreamStruct;
import jcl.lang.statics.ReaderVariables;
import jcl.util.CodePointConstants;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.DependsOn;
import org.springframework.stereotype.Component;

/**
 * Implements the '#\' Lisp reader macro.
 */
@Component
@DependsOn("readerBootstrap")
public class SharpBackslashReaderMacroFunction extends ReaderMacroFunctionImpl {

	private final ExtendedTokenReaderMacroFunction extendedTokenReaderMacroFunction;

	@Autowired
	public SharpBackslashReaderMacroFunction(final ExtendedTokenReaderMacroFunction extendedTokenReaderMacroFunction) {
		super("SHARP-BACKSLASH");
		this.extendedTokenReaderMacroFunction = extendedTokenReaderMacroFunction;
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
		ReaderVariables.READTABLE.getVariableValue().setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.BACKSLASH, this);
	}

	@Override
	public LispStruct readMacro(final ReaderInputStreamStruct inputStreamStruct, final int codePoint, final Optional<BigInteger> numberArgument) {
		assert codePoint == CodePointConstants.BACKSLASH;

		final ExtendedTokenReaderMacroFunction.ReadExtendedToken extendedToken = extendedTokenReaderMacroFunction.readExtendedToken(inputStreamStruct, true);
		final String tokenString = extendedToken.getTokenString();

		if (ReaderVariables.READ_SUPPRESS.getVariableValue().booleanValue()) {
			return NILStruct.INSTANCE;
		}

		final int maxTokenStringLength = 1;
		if (StringUtils.length(tokenString) == maxTokenStringLength) {
			final char characterToken = tokenString.charAt(0);
			return LispStructFactory.toCharacter(characterToken);
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
			nameCodePoint = UCharacter.getCharFromName(tokenString);
		}

		if (nameCodePoint == -1) {
			throw new ReaderErrorException("Unrecognized character name: " + tokenString);
		}

		return LispStructFactory.toCharacter(nameCodePoint);
	}
}
