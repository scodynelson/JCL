/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import jcl.LispStruct;
import jcl.lists.ConsStruct;
import jcl.lists.NullStruct;
import jcl.printer.LispPrinter;
import jcl.printer.Printer;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ConsStructPrinter implements LispPrinter<ConsStruct> {

	private static final long serialVersionUID = 2018276801770003739L;

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
		} else if (!object.getCdr().equals(NullStruct.INSTANCE)) {
			stringBuilder.append(" . ");

			final LispStruct cdr = object.getCdr();
			final String printedCdr = printer.print(cdr);

			stringBuilder.append(printedCdr);
		}

		return stringBuilder.toString();
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
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
