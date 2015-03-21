/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import jcl.numbers.IntegerStruct;
import jcl.numbers.RatioStruct;
import jcl.printer.LispPrinter;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class RatioStructPrinter implements LispPrinter<RatioStruct> {

	private static final long serialVersionUID = -491452763122265968L;

	@Autowired
	private IntegerStructPrinter integerStructPrinter;

	@Override
	public String print(final RatioStruct object) {
		final IntegerStruct numerator = new IntegerStruct(object.getBigFraction().getNumerator());
		final IntegerStruct denominator = new IntegerStruct(object.getBigFraction().getDenominator());

		return integerStructPrinter.print(numerator) + '/' + integerStructPrinter.print(denominator);
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public int hashCode() {
		return HashCodeBuilder.reflectionHashCode(this);
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public boolean equals(final Object obj) {
		return EqualsBuilder.reflectionEquals(this, obj);
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).toString();
	}
}
