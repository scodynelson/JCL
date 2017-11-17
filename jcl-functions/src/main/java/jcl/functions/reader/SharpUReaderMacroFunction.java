/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.functions.reader;

import java.math.BigInteger;
import java.util.Optional;

import jcl.lang.CharacterStruct;
import jcl.lang.InputStreamStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.ReadtableStruct;
import jcl.lang.statics.ReaderVariables;
import jcl.util.CodePointConstants;
import org.springframework.context.annotation.DependsOn;
import org.springframework.stereotype.Component;

/**
 * Implements the '#u' Lisp reader macro.
 */
@Component
@DependsOn("readerBootstrap")
public class SharpUReaderMacroFunction extends ReaderMacroFunctionImpl {

	public SharpUReaderMacroFunction() {
		super("SHARP-U");
	}

	@Override
	public void afterPropertiesSet() throws Exception {
		super.afterPropertiesSet();
		final ReadtableStruct readtable = ReaderVariables.READTABLE.getVariableValue();
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LATIN_SMALL_LETTER_U, this);
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LATIN_CAPITAL_LETTER_U, this);
	}

	@Override
	public LispStruct readMacro(final InputStreamStruct inputStreamStruct, final int codePoint, final Optional<BigInteger> numberArgument) {
		assert (codePoint == CodePointConstants.LATIN_SMALL_LETTER_U) || (codePoint == CodePointConstants.LATIN_CAPITAL_LETTER_U);

		if (ReaderVariables.READ_SUPPRESS.getVariableValue().toJavaPBoolean()) {
			return NILStruct.INSTANCE;
		}

		final int unicodeCodePoint = UnicodeCharacterReaderMacroFunction.readUnicodeCharacter(inputStreamStruct);
		return CharacterStruct.toLispCharacter(unicodeCodePoint);
	}
}
