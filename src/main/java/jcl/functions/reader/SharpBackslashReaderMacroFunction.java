/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import jcl.lang.CharacterStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.StringStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.statics.CommonLispSymbols;
import jcl.util.CodePointConstants;
import org.apache.commons.lang3.StringUtils;

/**
 * Implements the '#\' Lisp reader macro.
 */
public final class SharpBackslashReaderMacroFunction extends ReaderMacroFunctionImpl {

	public SharpBackslashReaderMacroFunction() {
		super("SHARP-BACKSLASH");
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint,
	                            final IntegerStruct numberArgument) {
		assert codePoint == CodePointConstants.BACKSLASH;

		final ExtendedTokenReaderMacroFunction.ReadExtendedToken extendedToken
				= ExtendedTokenReaderMacroFunction.readExtendedToken(inputStreamStruct, true);
		final String tokenString = extendedToken.getTokenString();

		if (CommonLispSymbols.READ_SUPPRESS_VAR.getVariableValue().toJavaPBoolean()) {
			return NILStruct.INSTANCE;
		}

		final int maxTokenStringLength = 1;
		if (StringUtils.length(tokenString) == maxTokenStringLength) {
			final char characterToken = tokenString.charAt(0);
			return CharacterStruct.toLispCharacter(characterToken);
		}

		final StringStruct string = StringStruct.toLispString(tokenString.toUpperCase());
		try {
			final LispStruct character = CharacterStruct.nameChar(string);
			if (NILStruct.INSTANCE.eq(character)) {
				throw new ReaderErrorException("Unrecognized character name: " + tokenString);
			}
			return character;
		} catch (final TypeErrorException ignored) {
			throw new ReaderErrorException("Unrecognized character name: " + tokenString);
		}
	}
}
