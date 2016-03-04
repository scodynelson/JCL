/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import jcl.LispStruct;
import jcl.lists.ConsStruct;
import jcl.printer.LispPrinter;
import jcl.printer.Printer;
import jcl.symbols.NILStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ConsStructPrinter implements LispPrinter<ConsStruct> {

	@Autowired
	private Printer printer;

	@Override
	public String print(final ConsStruct object) {
		// TODO: Ignoring *PRINT-PRETTY* and the pretty printer in general right now...

		if (object.isCircular()) {
			return "CIRCULAR LIST PRINTING NOT YET SUPPORTED!!!";
		}

		final StringBuilder stringBuilder = new StringBuilder();
		stringBuilder.append('(');

		printElements(object, stringBuilder);

		stringBuilder.append(')');

		return stringBuilder.toString();
	}

	private String printElements(final ConsStruct object, final StringBuilder stringBuilder) {

		final LispStruct car = object.getCar();
		final String printedCar = printer.print(car);

		stringBuilder.append(printedCar);

		if (object.getCdr() instanceof ConsStruct) {
			final ConsStruct cdrAsCons = (ConsStruct) object.getCdr();
			final String innerConsPrinted = printElements(cdrAsCons, new StringBuilder());

			stringBuilder.append(' ');
			stringBuilder.append(innerConsPrinted);
		} else if (!object.getCdr().equals(NILStruct.INSTANCE)) {
			stringBuilder.append(" . ");

			final LispStruct cdr = object.getCdr();
			final String printedCdr = printer.print(cdr);

			stringBuilder.append(printedCdr);
		}

		return stringBuilder.toString();
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
		final ConsStructPrinter rhs = (ConsStructPrinter) obj;
		return new EqualsBuilder().append(printer, rhs.printer)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(printer)
		                                                                .toString();
	}
}
