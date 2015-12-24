/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.math.BigInteger;
import java.util.Optional;
import javax.annotation.PostConstruct;

import jcl.LispStruct;
import jcl.characters.CharacterConstants;
import jcl.numbers.RationalStruct;
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
 * Implements the '#b' Lisp reader macro.
 */
@Component
public class SharpBReaderMacroFunction extends ReaderMacroFunction {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 4968332758845006560L;

	/**
	 * Radix value to use.
	 */
	private static final int RADIX = 2;

	/**
	 * {@link Autowired} {@link RationalReaderMacroFunction} used for reading {@link RationalStruct}s.
	 */
	@Autowired
	private RationalReaderMacroFunction rationalReaderMacroFunction;

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		final ReadtableStruct readtable = ReaderVariables.READTABLE.getVariableValue();
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_SMALL_LETTER_B, this);
		readtable.setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.LATIN_CAPITAL_LETTER_B, this);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Optional<BigInteger> numberArgument) {
		assert (codePoint == CharacterConstants.LATIN_SMALL_LETTER_B) || (codePoint == CharacterConstants.LATIN_CAPITAL_LETTER_B);

		return rationalReaderMacroFunction.readRational(reader, BigInteger.valueOf(RADIX));
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(rationalReaderMacroFunction)
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
		final SharpBReaderMacroFunction rhs = (SharpBReaderMacroFunction) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(rationalReaderMacroFunction, rhs.rationalReaderMacroFunction)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(rationalReaderMacroFunction)
		                                                                .toString();
	}
}
