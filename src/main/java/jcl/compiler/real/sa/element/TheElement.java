/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element;

import jcl.LispStruct;
import jcl.types.typespecifiers.TypeSpecifier;

public class TheElement implements Element {

	private final TypeSpecifier valueType;
	private final LispStruct form;

	public TheElement(final TypeSpecifier valueType, final LispStruct form) {
		this.valueType = valueType;
		this.form = form;
	}
}
