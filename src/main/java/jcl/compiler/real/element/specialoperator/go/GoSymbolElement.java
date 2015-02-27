/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator.go;

import jcl.compiler.real.element.SymbolElement;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class GoSymbolElement extends GoElement<SymbolElement> {

	private static final long serialVersionUID = -6696260185148126193L;

	public GoSymbolElement(final SymbolElement tag) {
		super(tag);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
