/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import java.math.BigInteger;
import java.util.Optional;
import javax.annotation.PostConstruct;

import jcl.lang.CharacterConstants;
import jcl.lang.LispStruct;
import jcl.lang.function.ReaderMacroFunction;
import jcl.lang.readtable.Reader;
import jcl.lang.readtable.ReaderVariables;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

/**
 * Implements the '#-' Lisp reader macro.
 */
@Component
public class SharpHyphenMinusReaderMacroFunction extends ReaderMacroFunction {

	/**
	 * {@link Autowired} {@link FeaturesReaderMacroFunction} used for reading features and either reading or
	 * suppressing the following {@link LispStruct}s based on whether or not the feature is present.
	 */
	@Autowired
	private FeaturesReaderMacroFunction featuresReaderMacroFunction;

	/**
	 * Initializes the reader macro function and adds it to the global readtable.
	 */
	@PostConstruct
	private void init() {
		ReaderVariables.READTABLE.getVariableValue().setDispatchMacroCharacter(CharacterConstants.NUMBER_SIGN, CharacterConstants.HYPHEN_MINUS, this);
	}

	@Override
	public LispStruct readMacro(final int codePoint, final Reader reader, final Optional<BigInteger> numberArgument) {
		assert codePoint == CharacterConstants.HYPHEN_MINUS;

		featuresReaderMacroFunction.readFeatures(reader, true);
		return null;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(featuresReaderMacroFunction)
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
		final SharpHyphenMinusReaderMacroFunction rhs = (SharpHyphenMinusReaderMacroFunction) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(featuresReaderMacroFunction, rhs.featuresReaderMacroFunction)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(featuresReaderMacroFunction)
		                                                                .toString();
	}
}
