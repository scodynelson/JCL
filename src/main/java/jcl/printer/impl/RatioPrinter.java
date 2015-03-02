/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

public abstract class RatioPrinter<O, P> implements LispPrinter<O> {

	private static final long serialVersionUID = 98246264280544825L;

	@Override
	public String print(final O object) {
		final P numerator = getNumerator(object);
		final P denominator = getDenominator(object);

		final IntegerPrinter<P> integerPrinter = getIntegerPrinter();

		return integerPrinter.print(numerator) + '/' + integerPrinter.print(denominator);
	}

	protected abstract P getNumerator(O object);

	protected abstract P getDenominator(O object);

	protected abstract IntegerPrinter<P> getIntegerPrinter();
}
