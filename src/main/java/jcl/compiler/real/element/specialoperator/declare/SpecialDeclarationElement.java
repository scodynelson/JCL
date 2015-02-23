/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator.declare;

import jcl.compiler.real.element.SymbolElement;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class SpecialDeclarationElement implements DeclarationElement {

	private static final long serialVersionUID = 7353657327204677544L;

	private final SymbolElement var;

	public SpecialDeclarationElement(final SymbolElement var) {
		this.var = var;
	}

	public SymbolElement getVar() {
		return var;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
