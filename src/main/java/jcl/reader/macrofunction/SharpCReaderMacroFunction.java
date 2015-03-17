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
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.numbers.RealStruct;
import jcl.printer.Printer;
import jcl.reader.Reader;
import jcl.reader.ReaderMacroFunction;
import jcl.reader.struct.ReaderVariables;
import jcl.reader.struct.ReadtableStruct;
import jcl.system.CommonLispSymbols;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Implements the '#c' Lisp reader macro.
 */
@Component
public class SharpCReaderMacroFunction extends ReaderMacroFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -2703333209830257710L;

	/**
	 * {@link Autowired} {@link Printer} used for printing elements and structures to the output stream.
	 */
	@Autowired
	private Printer printer;

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		final ReadtableStruct readtable = ReaderVariables.READTABLE.getValue();
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_SMALL_LETTER_C, this);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_CAPITAL_LETTER_C, this);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Optional<BigInteger> numberArgument) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_C) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_C);

		final LispStruct token = reader.read(true, NullStruct.INSTANCE, true);
		if (ReaderVariables.READ_SUPPRESS.getValue().booleanValue()) {
			return NullStruct.INSTANCE;
		}

		if (!(token instanceof ListStruct)) {
			final String printedToken = printer.print(token);
			throw new ReaderErrorException("Illegal complex number format: #C" + printedToken);
		}

		final ListStruct listToken = (ListStruct) token;
		if (!listToken.isProper()) {
			final String printedToken = printer.print(token);
			throw new ReaderErrorException("Illegal complex number format: #C" + printedToken);
		}

		final int maxNumberOfTokensForComplex = 2;
		if (listToken.size() != maxNumberOfTokensForComplex) {
			final String printedToken = printer.print(token);
			throw new ReaderErrorException("Illegal complex number format: #C" + printedToken);
		}

		final LispStruct realToken = listToken.getFirst();
		if (!(realToken instanceof RealStruct)) {
			final String printedRealToken = printer.print(realToken);
			throw new ReaderErrorException("Only real numbers are valid tokens for #c. Got: " + printedRealToken);
		}

		final LispStruct imaginaryToken = listToken.getRest().getFirst();
		if (!(imaginaryToken instanceof RealStruct)) {
			final String printedImaginaryToken = printer.print(imaginaryToken);
			throw new ReaderErrorException("Only real numbers are valid tokens for #c. Got: " + printedImaginaryToken);
		}

		return ListStruct.buildProperList(CommonLispSymbols.COMPLEX, realToken, imaginaryToken);
	}
}
