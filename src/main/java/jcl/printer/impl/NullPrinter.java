/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

public abstract class NullPrinter<O> implements LispPrinter<O> {

	private static final long serialVersionUID = 2302849748345497770L;

	@Override
	public String print(final O object) {
		return "NIL";
	}
}
