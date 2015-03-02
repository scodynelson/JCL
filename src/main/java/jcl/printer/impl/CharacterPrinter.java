/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import jcl.printer.PrinterVariables;

public abstract class CharacterPrinter<O> implements LispPrinter<O> {

	private static final long serialVersionUID = 1389513684443461697L;

	@Override
	public String print(final O object) {
		final boolean printEscape = PrinterVariables.PRINT_ESCAPE.getValue().booleanValue();

		final StringBuilder stringBuilder = new StringBuilder();
		if (printEscape) {
			stringBuilder.append("#\\");
		}

		final int codePoint = getCodePoint(object);

		if (Character.isLetterOrDigit(codePoint)) {
			stringBuilder.append(codePoint);
		} else {
			stringBuilder.append(Character.getName(codePoint));
		}

		return stringBuilder.toString();
	}

	protected abstract int getCodePoint(O object);
}
