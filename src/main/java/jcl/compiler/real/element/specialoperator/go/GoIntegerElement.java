/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator.go;

import jcl.numbers.IntegerStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class GoIntegerElement extends GoElement<IntegerStruct> {

	private static final long serialVersionUID = 6515586661046207604L;

	public GoIntegerElement(final IntegerStruct tag) {
		super(tag);
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
