/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import jcl.printer.LispPrinter;
import jcl.printer.Printer;
import jcl.streams.StreamStruct;
import jcl.streams.SynonymStreamStruct;
import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class SynonymStreamStructPrinter implements LispPrinter<SynonymStreamStruct> {

	private static final long serialVersionUID = 1464134062277994189L;

	@Autowired
	private Printer printer;

	@Override
	public String print(final SynonymStreamStruct object) {
		final String typeClassName = object.getType().getClass().getName().toUpperCase();

		final SymbolStruct<StreamStruct> symbol = object.getSymbol();
		final String printedSymbol = printer.print(symbol);

		return "#<" + typeClassName + " to " + printedSymbol + '>';
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
		final SynonymStreamStructPrinter rhs = (SynonymStreamStructPrinter) obj;
		return new EqualsBuilder().append(printer, rhs.printer)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(printer)
		                                                                .toString();
	}
}
