/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import jcl.lang.CharacterStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.statics.CommonLispSymbols;
import jcl.util.CodePointConstants;

/**
 * Implements the '#u' Lisp reader macro.
 */
public final class SharpUReaderMacroFunction extends ReaderMacroFunctionImpl {

	public SharpUReaderMacroFunction() {
		super("SHARP-U");
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint,
	                            final IntegerStruct numberArgument) {
		assert (codePoint == CodePointConstants.LATIN_SMALL_LETTER_U) || (codePoint == CodePointConstants.LATIN_CAPITAL_LETTER_U);

		if (CommonLispSymbols.READ_SUPPRESS_VAR.getVariableValue().toJavaPBoolean()) {
			ExtendedTokenReaderMacroFunction.readExtendedToken(inputStreamStruct, false);
			return NILStruct.INSTANCE;
		}

		final int unicodeCodePoint = UnicodeCharacterReaderMacroFunction.readUnicodeCharacter(inputStreamStruct);
		return CharacterStruct.toLispCharacter(unicodeCodePoint);
	}
}
