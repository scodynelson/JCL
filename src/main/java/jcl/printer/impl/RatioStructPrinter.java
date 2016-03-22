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

	@Autowired
	private IntegerStructPrinter integerStructPrinter;

	@Override
	public String print(final RatioStruct object) {
		final IntegerStruct numerator = IntegerStruct.valueOf(object.getBigFraction().getNumerator());
		final IntegerStruct denominator = IntegerStruct.valueOf(object.getBigFraction().getDenominator());

		return integerStructPrinter.print(numerator) + '/' + integerStructPrinter.print(denominator);
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(integerStructPrinter)
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
		final RatioStructPrinter rhs = (RatioStructPrinter) obj;
		return new EqualsBuilder().append(integerStructPrinter, rhs.integerStructPrinter)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(integerStructPrinter)
		                                                                .toString();
	}
}
