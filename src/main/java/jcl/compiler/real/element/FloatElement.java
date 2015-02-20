/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element;

import jcl.numbers.FloatStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class FloatElement implements Element {

	private static final long serialVersionUID = 9120922223192617896L;

	private final FloatStruct floatStruct;

	public FloatElement(final FloatStruct floatStruct) {
		this.floatStruct = floatStruct;
	}

	public FloatStruct getFloatStruct() {
		return floatStruct;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
