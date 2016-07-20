/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.math.BigInteger;
import java.util.Optional;
import javax.annotation.PostConstruct;

import jcl.lang.LispStruct;
import jcl.lang.character.CharacterStruct;
import jcl.lang.function.ReaderMacroFunction;
import jcl.lang.list.NILStruct;
import jcl.lang.readtable.Reader;
import jcl.lang.readtable.ReaderVariables;
import jcl.lang.readtable.ReadtableStruct;
import jcl.util.CodePointConstants;
import org.springframework.stereotype.Component;

/**
 * Implements the '#u' Lisp reader macro.
 */
@Component
public class SharpUReaderMacroFunction extends ReaderMacroFunction {

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		final ReadtableStruct readtable = ReaderVariables.READTABLE.getVariableValue();
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LATIN_SMALL_LETTER_U, this);
		readtable.setDispatchMacroCharacter(CodePointConstants.NUMBER_SIGN, CodePointConstants.LATIN_CAPITAL_LETTER_U, this);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Optional<BigInteger> numberArgument) {
		assert (codePoint == CodePointConstants.LATIN_SMALL_LETTER_U) || (codePoint == CodePointConstants.LATIN_CAPITAL_LETTER_U);

		if (ReaderVariables.READ_SUPPRESS.getVariableValue().booleanValue()) {
			return NILStruct.INSTANCE;
		}

		final int unicodeCodePoint = UnicodeCharacterReaderMacroFunction.readUnicodeCharacter(reader);
		return CharacterStruct.valueOf(unicodeCodePoint);
	}
}
