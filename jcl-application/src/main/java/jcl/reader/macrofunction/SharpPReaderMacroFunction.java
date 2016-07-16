/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.math.BigInteger;
import java.util.Optional;
import javax.annotation.PostConstruct;

import jcl.lang.CharacterConstants;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.array.StringStruct;
import jcl.lang.condition.exception.ReaderErrorException;
import jcl.lang.function.ReaderMacroFunction;
import jcl.lang.pathname.PathnameStruct;
import jcl.lang.readtable.Reader;
import jcl.lang.readtable.ReaderVariables;
import jcl.lang.readtable.ReadtableStruct;
import jcl.pathnames.functions.PathnameFunction;
import jcl.printer.Printer;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Implements the '#p' Lisp reader macro.
 */
@Component
public class SharpPReaderMacroFunction extends ReaderMacroFunction {

	/**
	 * {@link Autowired} {@link Printer} used for printing elements and structures to the output stream.
	 */
	@Autowired
	private Printer printer;

	/**
	 * {@link Autowired} {@link PathnameFunction} used for getting a new {@link PathnameStruct} instance from the read
	 * in pathname namestring.
	 */
	@Autowired
	private PathnameFunction pathnameFunction;

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		final ReadtableStruct readtable = ReaderVariables.READTABLE.getVariableValue();
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_SMALL_LETTER_P, this);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_CAPITAL_LETTER_P, this);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Optional<BigInteger> numberArgument) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_P) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_P);

		final LispStruct token = reader.read(true, NILStruct.INSTANCE, true);
		if (ReaderVariables.READ_SUPPRESS.getVariableValue().booleanValue()) {
			return NILStruct.INSTANCE;
		}

		if (token instanceof StringStruct) {
			final StringStruct pathnameString = (StringStruct) token;
			return pathnameFunction.pathname(pathnameString);
		} else {
			final String printedToken = printer.print(token);
			throw new ReaderErrorException("The value " + printedToken + " is not of expected type STRING in argument to #P.");
		}
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(printer)
		                            .append(pathnameFunction)
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
		final SharpPReaderMacroFunction rhs = (SharpPReaderMacroFunction) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(printer, rhs.printer)
		                          .append(pathnameFunction, rhs.pathnameFunction)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(printer)
		                                                                .append(pathnameFunction)
		                                                                .toString();
	}
}
