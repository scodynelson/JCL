/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import jcl.printer.PrinterVariables;

import java.math.BigInteger;

public abstract class IntegerPrinter<O> implements LispPrinter<O> {

	/**
	 * Int constant for the value '2'.
	 */
	private static final int TWO = 2;

	/**
	 * Int constant for the value '8'.
	 */
	private static final int EIGHT = 8;

	/**
	 * Int constant for the value '10'.
	 */
	private static final int TEN = 10;

	/**
	 * Int constant for the value '16'.
	 */
	private static final int SIXTEEN = 16;

	private static final long serialVersionUID = 1051464035751713538L;

	@Override
	public String print(final O object) {
		final boolean printRadix = PrinterVariables.PRINT_RADIX.getValue().booleanValue();
		final int printBase = PrinterVariables.PRINT_BASE.getValue().getBigInteger().intValue();

		final StringBuilder stringBuilder = new StringBuilder();

		if (printRadix) {
			if (printBase == TWO) {
				stringBuilder.append("#b");
			} else if (printBase == EIGHT) {
				stringBuilder.append("#o");
			} else if (printBase == SIXTEEN) {
				stringBuilder.append("#x");
			} else {
				stringBuilder.append('#');
				stringBuilder.append(printBase);
				stringBuilder.append('r');
			}
		}

		final BigInteger bigInteger = getBigInteger(object);
		stringBuilder.append(bigInteger.toString(printBase));

		if (printRadix && (printBase == TEN)) {
			stringBuilder.append('.');
		}

		return stringBuilder.toString();
	}

	protected abstract BigInteger getBigInteger(O object);
}
