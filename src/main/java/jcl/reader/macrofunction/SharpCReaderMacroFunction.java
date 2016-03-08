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
import jcl.numbers.ComplexStruct;
import jcl.numbers.RealStruct;
import jcl.printer.Printer;
import jcl.reader.Reader;
import jcl.reader.ReaderMacroFunction;
import jcl.reader.struct.ReaderVariables;
import jcl.reader.struct.ReadtableStruct;
import jcl.symbols.NILStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Implements the '#c' Lisp reader macro.
 */
@Component
public class SharpCReaderMacroFunction extends ReaderMacroFunction {

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

		final LispStruct realToken = listToken.getCar();
		if (!(realToken instanceof RealStruct)) {
			final String printedRealToken = printer.print(realToken);
			throw new ReaderErrorException("Only real numbers are valid tokens for #c. Got: " + printedRealToken);
		}

		final LispStruct imaginaryToken = listToken.getRest().getCar();
		if (!(imaginaryToken instanceof RealStruct)) {
			final String printedImaginaryToken = printer.print(imaginaryToken);
			throw new ReaderErrorException("Only real numbers are valid tokens for #c. Got: " + printedImaginaryToken);
		}

		return new ComplexStruct((RealStruct) realToken, (RealStruct) imaginaryToken);
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(printer)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final SharpCReaderMacroFunction rhs = (SharpCReaderMacroFunction) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(printer, rhs.printer)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(printer)
		                                                                .toString();
	}
}
