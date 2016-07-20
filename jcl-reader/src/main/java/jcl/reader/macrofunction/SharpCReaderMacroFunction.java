/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.math.BigInteger;
import java.util.Iterator;
import java.util.Optional;
import javax.annotation.PostConstruct;

import jcl.lang.character.CharacterConstants;
import jcl.lang.LispStruct;
import jcl.lang.list.ListStruct;
import jcl.lang.list.NILStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.function.ReaderMacroFunction;
import jcl.lang.number.ComplexStruct;
import jcl.lang.number.RealStruct;
import jcl.lang.readtable.Reader;
import jcl.lang.readtable.ReaderVariables;
import jcl.lang.readtable.ReadtableStruct;
import org.springframework.stereotype.Component;

/**
 * Implements the '#c' Lisp reader macro.
 */
@Component
public class SharpCReaderMacroFunction extends ReaderMacroFunction {

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		final ReadtableStruct readtable = ReaderVariables.READTABLE.getVariableValue();
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_SMALL_LETTER_C, this);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_CAPITAL_LETTER_C, this);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Optional<BigInteger> numberArgument) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_C) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_C);

		final LispStruct token = reader.read(true, NILStruct.INSTANCE, true);
		if (ReaderVariables.READ_SUPPRESS.getVariableValue().booleanValue()) {
			return NILStruct.INSTANCE;
		}

		if (!(token instanceof ListStruct)) {
			throw new ReaderErrorException("Illegal complex number format: #C" + token);
		}

		final ListStruct listToken = (ListStruct) token;
		if (!listToken.isProper()) {
			throw new ReaderErrorException("Illegal complex number format: #C" + token);
		}

		final int maxNumberOfTokensForComplex = 2;
		if (listToken.length() != maxNumberOfTokensForComplex) {
			throw new ReaderErrorException("Illegal complex number format: #C" + token);
		}
		final Iterator<LispStruct> iterator = listToken.iterator();

		final LispStruct realToken = iterator.next();
		if (!(realToken instanceof RealStruct)) {
			throw new ReaderErrorException("Only real numbers are valid tokens for #c. Got: " + realToken);
		}

		final LispStruct imaginaryToken = iterator.next();
		if (!(imaginaryToken instanceof RealStruct)) {
			throw new ReaderErrorException("Only real numbers are valid tokens for #c. Got: " + imaginaryToken);
		}

		return ComplexStruct.valueOf((RealStruct) realToken, (RealStruct) imaginaryToken);
	}
}
