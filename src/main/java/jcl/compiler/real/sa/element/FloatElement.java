/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.sa.element;

import jcl.numbers.FloatStruct;

public class FloatElement implements Element {

	private static final long serialVersionUID = 9120922223192617896L;

	private final FloatStruct floatStruct;

	public FloatElement(final FloatStruct floatStruct) {
		this.floatStruct = floatStruct;
	}

	public FloatStruct getFloatStruct() {
		return floatStruct;
	}
}
