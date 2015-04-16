/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import java.util.Map;

import jcl.LispStruct;
import jcl.printer.LispPrinter;
import jcl.printer.Printer;
import jcl.structures.StructureObjectStruct;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class StructureObjectStructPrinter<TYPE extends StructureObjectStruct> implements LispPrinter<TYPE> {

	private static final long serialVersionUID = 1860328949714109890L;

	@Autowired
	private Printer printer;

	@Override
	public String print(final TYPE object) {
		final StringBuilder stringBuilder = new StringBuilder();

		stringBuilder.append("#S(");

		final SymbolStruct<?> structureSymbol = object.getStructureSymbol();
		final String printedStructureSymbol = printer.print(structureSymbol);
		stringBuilder.append(printedStructureSymbol);

		final Map<SymbolStruct<?>, LispStruct> slots = object.getSlots();
		for (final Map.Entry<SymbolStruct<?>, LispStruct> slot : slots.entrySet()) {
			final SymbolStruct<?> slotSymbol = slot.getKey();
			final LispStruct slotValue = slot.getValue();

			stringBuilder.append(" :");
			final String printedSlotSymbol = printer.print(slotSymbol);
			stringBuilder.append(printedSlotSymbol);

			stringBuilder.append(' ');
			final String printedSlotValue = printer.print(slotValue);
			stringBuilder.append(printedSlotValue);
		}

		stringBuilder.append(')');

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
		final StructureObjectStructPrinter<?> rhs = (StructureObjectStructPrinter) obj;
		return new EqualsBuilder().append(printer, rhs.printer)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(printer)
		                                                                .toString();
	}
}
