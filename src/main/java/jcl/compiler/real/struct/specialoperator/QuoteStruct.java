/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.struct.SpecialOperatorStruct;

public class QuoteStruct extends SpecialOperatorStruct {

	private static final long serialVersionUID = 2854755653951600124L;

	private final LispStruct object;

	public QuoteStruct(final LispStruct object) {
		this.object = object;
	}

	public LispStruct getObject() {
		return object;
	}
}
