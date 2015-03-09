/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.math.BigInteger;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.characters.CharacterConstants;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.lists.ListStruct;
import jcl.lists.NullStruct;
import jcl.numbers.RealStruct;
import jcl.printer.Printer;
import jcl.reader.Reader;
import jcl.reader.struct.ReaderVariables;
import jcl.reader.struct.ReadtableStruct;
import jcl.system.CommonLispSymbols;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Implements the '#c' Lisp reader macro.
 */
@Component
public class SharpCReaderMacroFunction extends ReaderMacroFunctionImpl {

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
	public LispStruct readMacro(final int codePoint, final Reader reader, final BigInteger numArg) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_C) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_C);

		final LispStruct lispToken = reader.read(true, NullStruct.INSTANCE, true);
		if (ReaderVariables.READ_SUPPRESS.getValue().booleanValue()) {
			return NullStruct.INSTANCE;
		}

		if (!(lispToken instanceof ListStruct)) {
			final String printedToken = printer.print(lispToken);
			throw new ReaderErrorException("Illegal complex number format: #C" + printedToken);
		}

		final ListStruct listToken = (ListStruct) lispToken;
		if (!listToken.isProper()) {
			final String printedToken = printer.print(lispToken);
			throw new ReaderErrorException("Illegal complex number format: #C" + printedToken);
		}

		final int maxNumberOfTokensForComplex = 2;
		if (listToken.size() != maxNumberOfTokensForComplex) {
			final String printedToken = printer.print(lispToken);
			throw new ReaderErrorException("Illegal complex number format: #C" + printedToken);
		}

		final LispStruct real = listToken.getFirst();
		if (!(real instanceof RealStruct)) {
			final String printedReal = printer.print(real);
			throw new ReaderErrorException("Only real numbers are valid tokens for #c. Got: " + printedReal);
		}

		final LispStruct imaginary = listToken.getRest().getFirst();
		if (!(imaginary instanceof RealStruct)) {
			final String printedImaginary = printer.print(imaginary);
			throw new ReaderErrorException("Only real numbers are valid tokens for #c. Got: " + printedImaginary);
		}

		return ListStruct.buildProperList(CommonLispSymbols.COMPLEX, real, imaginary);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
