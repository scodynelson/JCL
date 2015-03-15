/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import jcl.numbers.ComplexStruct;
import jcl.numbers.RealStruct;
import jcl.printer.Printer;
import jcl.printer.LispPrinter;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ComplexStructPrinter implements LispPrinter<ComplexStruct> {

	private static final long serialVersionUID = 948389112917380146L;

	@Autowired
	private Printer printer;

	@Override
	public String print(final ComplexStruct object) {
		final RealStruct real = object.getReal();
		final String printedReal = printer.print(real);

		final RealStruct imaginary = object.getImaginary();
		final String printedImaginary = printer.print(imaginary);

		return "#C(" +printedReal + ' ' + printedImaginary + ')';
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
