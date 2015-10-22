/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.struct.specialoperator;

import jcl.LispStruct;
import jcl.compiler.real.struct.CompilerSpecialOperatorStruct;

public class QuoteStruct extends CompilerSpecialOperatorStruct {

	private static final long serialVersionUID = 2854755653951600124L;

	private final LispStruct object;

	public QuoteStruct(final LispStruct object) {
		this.object = object;
	}

	public LispStruct getObject() {
		return object;
	}
}
