/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl.struct;

import jcl.arrays.BitVectorStruct;
import jcl.numbers.IntegerStruct;
import jcl.printer.Printer;
import jcl.printer.PrinterVariables;
import jcl.printer.impl.LispPrinter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;

@Component
public class BitVectorStructPrinter implements LispPrinter<BitVectorStruct> {

	private static final long serialVersionUID = -1549143424753157961L;

	@Autowired
	private Printer printer;

	@Override
	public String print(final BitVectorStruct object) {
		final boolean printArray = PrinterVariables.PRINT_ARRAY.getValue().booleanValue();

		final StringBuilder stringBuilder = new StringBuilder();

		final Integer fillPointer = object.getFillPointer();
		final List<IntegerStruct> contents = object.getContents();

		if (printArray) {
			stringBuilder.append("#*");

			final int amountToPrint = (fillPointer == null) ? contents.size() : fillPointer;

			for (int i = 0; i < amountToPrint; i++) {
				final IntegerStruct integerStruct = contents.get(i);
				final String printedIntegerStruct = printer.print(integerStruct);

				stringBuilder.append(printedIntegerStruct);
			}
		} else {
			final String typeClassName = object.getType().getClass().getName().toUpperCase();

			stringBuilder.append("#<");
			stringBuilder.append(typeClassName);
			stringBuilder.append(' ');

			final int totalSize = object.getTotalSize();
			stringBuilder.append(totalSize);

			if (fillPointer != null) {
				stringBuilder.append(" fill-pointer ");
				stringBuilder.append(fillPointer);
			}

			final boolean isAdjustable = object.isAdjustable();
			if (isAdjustable) {
				stringBuilder.append(" adjustable");
			}

			stringBuilder.append('>');
		}

		return stringBuilder.toString();
	}
}
