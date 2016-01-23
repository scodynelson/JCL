/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import jcl.LispStruct;
import jcl.compiler.struct.CompilerSpecialOperatorStruct;

public class QuoteStruct extends CompilerSpecialOperatorStruct {

	private final LispStruct object;

	public QuoteStruct(final LispStruct object) {
		this.object = object;
	}

	public LispStruct getObject() {
		return object;
	}
}
