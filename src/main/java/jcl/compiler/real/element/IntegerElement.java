/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element;

import jcl.numbers.IntegerStruct;

public class IntegerElement implements Element {

	private static final long serialVersionUID = 4314662992381073176L;

	private final IntegerStruct integerStruct;

	public IntegerElement(final IntegerStruct integerStruct) {
		this.integerStruct = integerStruct;
	}

	public IntegerStruct getIntegerStruct() {
		return integerStruct;
	}
}
