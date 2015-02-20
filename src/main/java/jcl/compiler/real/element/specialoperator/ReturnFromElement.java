/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator;

import jcl.compiler.real.element.Element;
import jcl.compiler.real.element.NullElement;
import jcl.compiler.real.element.SymbolElement;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class ReturnFromElement implements Element {

	private static final long serialVersionUID = -6095397540807480492L;

	private final SymbolElement<?> name;
	private final Element result;

	public ReturnFromElement(final SymbolElement<?> name) {
		this.name = name;
		result = NullElement.INSTANCE;
	}

	public ReturnFromElement(final SymbolElement<?> name, final Element result) {
		this.name = name;
		this.result = result;
	}

	public SymbolElement<?> getName() {
		return name;
	}

	public Element getResult() {
		return result;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
