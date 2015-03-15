/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import jcl.printer.Printer;
import jcl.printer.LispPrinter;
import jcl.streams.EchoStreamStruct;
import jcl.streams.InputStream;
import jcl.streams.OutputStream;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class EchoStreamStructPrinter implements LispPrinter<EchoStreamStruct> {

	private static final long serialVersionUID = 5872440805729953534L;

	@Autowired
	private Printer printer;

	@Override
	public String print(final EchoStreamStruct object) {
		final String typeClassName = object.getType().getClass().getName().toUpperCase();

		final InputStream inputStream = object.getInputStream();
		final String printedInputStream = printer.print(inputStream);

		final OutputStream outputStream = object.getOutputStream();
		final String printedOutputStream = printer.print(outputStream);

		return "#<" + typeClassName + " input " + printedInputStream + ", output " + printedOutputStream + '>';
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
