/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import jcl.numbers.ComplexStruct;
import jcl.numbers.RealStruct;
import jcl.printer.LispPrinter;
import jcl.printer.Printer;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ComplexStructPrinter implements LispPrinter<ComplexStruct> {

	@Autowired
	private Printer printer;

	@Override
	public String print(final ComplexStruct object) {
		final RealStruct real = object.realPart();
		final String printedReal = printer.print(real);

		final RealStruct imaginary = object.imagPart();
		final String printedImaginary = printer.print(imaginary);

		return "#C(" + printedReal + ' ' + printedImaginary + ')';
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(printer)
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
		final ComplexStructPrinter rhs = (ComplexStructPrinter) obj;
		return new EqualsBuilder().append(printer, rhs.printer)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(printer)
		                                                                .toString();
	}
}
