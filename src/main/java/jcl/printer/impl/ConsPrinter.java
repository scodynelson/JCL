/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

public abstract class ConsPrinter<O> implements LispPrinter<O> {

	@Override
	public String print(final O object) {
		// TODO: Ignoring *PRINT-PRETTY* and the pretty printer in general right now...

		if (isCircular(object)) {
			return "CIRCULAR LIST PRINTING NOT YET SUPPORTED!!!";
		}

		final StringBuilder stringBuilder = new StringBuilder();
		stringBuilder.append('(');

		printElements(object, stringBuilder);

		stringBuilder.append(')');

		return stringBuilder.toString();
	}

	protected abstract boolean isCircular(O object);

	protected abstract void printElements(O object, StringBuilder stringBuilder);
}
