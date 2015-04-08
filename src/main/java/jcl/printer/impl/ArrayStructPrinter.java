/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import java.util.List;

import jcl.LispStruct;
import jcl.LispType;
import jcl.arrays.ArrayStruct;
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
public class ArrayStructPrinter<TYPE extends LispStruct> implements LispPrinter<ArrayStruct<TYPE>> {

	private static final long serialVersionUID = -4715974488949942878L;

	@Autowired
	private Printer printer;

	@Override
	public String print(final ArrayStruct<TYPE> object) {
		// TODO: Ignoring *PRINT-LEVEL* and *PRINT-LENGTH*

		final boolean printArray = PrinterVariables.PRINT_ARRAY.getValue().booleanValue();
		final boolean printReadably = PrinterVariables.PRINT_READABLY.getValue().booleanValue();

		final StringBuilder stringBuilder = new StringBuilder();

		if (printArray || printReadably) {
			stringBuilder.append('#');

			final int rank = object.getRank();
			stringBuilder.append(rank);
			stringBuilder.append("A(");

			final int totalSize = object.getTotalSize();
			final List<TYPE> contents = object.getContents();

			for (int i = 0; i < totalSize; i++) {
				final TYPE lispStruct = contents.get(i);
				final String printedLispStruct = printer.print(lispStruct);

				stringBuilder.append(printedLispStruct);

				if (i < (totalSize - 1)) {
					stringBuilder.append(' ');
				}
			}

			stringBuilder.append(')');
		} else {
			final String typeClassName = object.getType().getClass().getName().toUpperCase();

			stringBuilder.append("#<");
			stringBuilder.append(typeClassName);
			stringBuilder.append(' ');

			final List<Integer> dimensions = object.getDimensions();

			for (int i = 0; i < dimensions.size(); i++) {
				stringBuilder.append(dimensions.get(i));

				if ((i + 1) != dimensions.size()) {
					stringBuilder.append('x');
				}
			}

			stringBuilder.append(" type ");

			final LispType elementType = object.getElementType();
			final String elementTypeClassName = elementType.getClass().getName().toUpperCase();
			stringBuilder.append(elementTypeClassName);

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
		final ArrayStructPrinter<?> rhs = (ArrayStructPrinter) obj;
		return new EqualsBuilder().append(printer, rhs.printer)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(printer)
		                                                                .toString();
	}
}
