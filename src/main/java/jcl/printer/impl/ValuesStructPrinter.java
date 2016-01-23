/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import java.util.List;

import jcl.LispStruct;
import jcl.compiler.struct.ValuesStruct;
import jcl.printer.LispPrinter;
import jcl.printer.Printer;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ValuesStructPrinter implements LispPrinter<ValuesStruct> {

	@Autowired
	private Printer printer;

	@Override
	public String print(final ValuesStruct object) {

		final StringBuilder stringBuilder = new StringBuilder();

		final List<LispStruct> valuesList = object.getValuesList();
		final int numberOfValues = valuesList.size();

		for (int i = 0; i < numberOfValues; i++) {
			final LispStruct value = valuesList.get(i);

			final String printedValue = printer.print(value);
			stringBuilder.append(printedValue);

			if (i < (numberOfValues - 1)) {
				stringBuilder.append('\n');
			}
		}

		return stringBuilder.toString();
	}
}
