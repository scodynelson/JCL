/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import jcl.printer.Printer;
import jcl.printer.LispPrinter;
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
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).toString();
	}
}
