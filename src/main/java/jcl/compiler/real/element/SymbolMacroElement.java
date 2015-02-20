/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element;

import jcl.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class SymbolMacroElement implements Element {

	private static final long serialVersionUID = -1618487345643376983L;

	private final SymbolStruct<?> symbolStruct;

	private final Element form;

	public SymbolMacroElement(final SymbolStruct<?> symbolStruct, final Element form) {
		this.symbolStruct = symbolStruct;
		this.form = form;
	}

	public SymbolStruct<?> getSymbolStruct() {
		return symbolStruct;
	}

	public Element getForm() {
		return form;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
