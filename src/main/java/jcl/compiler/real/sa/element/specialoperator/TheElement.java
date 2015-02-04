/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element.specialoperator;

import jcl.compiler.real.sa.element.Element;
import jcl.types.typespecifiers.TypeSpecifier;

public class TheElement implements Element {

	private static final long serialVersionUID = -8054543185157500625L;

	private final TypeSpecifier valueType;
	private final Element form;

	public TheElement(final TypeSpecifier valueType, final Element form) {
		this.valueType = valueType;
		this.form = form;
	}

	public TypeSpecifier getValueType() {
		return valueType;
	}

	public Element getForm() {
		return form;
	}
}
