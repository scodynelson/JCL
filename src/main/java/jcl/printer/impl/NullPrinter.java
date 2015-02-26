/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

public abstract class NullPrinter<O> implements LispPrinter<O> {

	@Override
	public String print(final O object) {
		return "NIL";
	}
}
