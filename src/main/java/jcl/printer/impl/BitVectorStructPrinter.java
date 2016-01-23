/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import java.util.List;

import jcl.arrays.BitVectorStruct;
import jcl.numbers.IntegerStruct;
import jcl.printer.LispPrinter;
import jcl.printer.Printer;
import jcl.printer.PrinterVariables;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class BitVectorStructPrinter implements LispPrinter<BitVectorStruct> {

	@Autowired
	private Printer printer;

	@Override
	public String print(final BitVectorStruct object) {
		final boolean printArray = PrinterVariables.PRINT_ARRAY.getVariableValue().booleanValue();

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
			final String typeClassName = object.getType().getClass().getSimpleName().toUpperCase();

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
		final BitVectorStructPrinter rhs = (BitVectorStructPrinter) obj;
		return new EqualsBuilder().append(printer, rhs.printer)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(printer)
		                                                                .toString();
	}
}
