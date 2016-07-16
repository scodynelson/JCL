/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.struct.specialoperator;

import jcl.compiler.struct.CompilerSpecialOperatorStruct;
import jcl.lang.LispStruct;

public class QuoteStruct extends CompilerSpecialOperatorStruct {

	private final LispStruct object;

	public QuoteStruct(final LispStruct object) {
		this.object = object;
	}

	public LispStruct getObject() {
		return object;
	}

	@Override
	public String toString() {
		final StringBuilder builder = new StringBuilder("'");

		final String objectPrinted = object.toString();
		builder.append(objectPrinted);

		return builder.toString();
	}
}
