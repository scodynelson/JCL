/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.math.BigInteger;
import java.util.Optional;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.arrays.StringStruct;
import jcl.characters.CharacterConstants;
import jcl.conditions.exceptions.ReaderErrorException;
import jcl.lists.NullStruct;
import jcl.pathnames.PathnameStruct;
import jcl.pathnames.functions.PathnameFunction;
import jcl.printer.Printer;
import jcl.reader.Reader;
import jcl.reader.ReaderMacroFunction;
import jcl.reader.struct.ReaderVariables;
import jcl.reader.struct.ReadtableStruct;
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
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 3962629854177635283L;

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

		final LispStruct token = reader.read(true, NullStruct.INSTANCE, true);
		if (ReaderVariables.READ_SUPPRESS.getVariableValue().booleanValue()) {
			return NullStruct.INSTANCE;
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
