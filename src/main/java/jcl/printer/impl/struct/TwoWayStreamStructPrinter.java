/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl.struct;

import jcl.printer.Printer;
import jcl.printer.impl.LispPrinter;
import jcl.streams.InputStream;
import jcl.streams.OutputStream;
import jcl.streams.TwoWayStreamStruct;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class TwoWayStreamStructPrinter implements LispPrinter<TwoWayStreamStruct> {

	private static final long serialVersionUID = -7785883762162329655L;

	@Autowired
	private Printer printer;

	@Override
	public String print(final TwoWayStreamStruct object) {
		final String typeClassName = object.getType().getClass().getName().toUpperCase();

		final InputStream inputStream = object.getInputStream();
		final String printedInputStream = printer.print(inputStream);

		final OutputStream outputStream = object.getOutputStream();
		final String printedOutputStream = printer.print(outputStream);

		return "#<" + typeClassName + " input " + printedInputStream + ", output " + printedOutputStream + '>';
	}
}
