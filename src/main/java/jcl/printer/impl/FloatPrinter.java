/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import jcl.reader.struct.ReaderVariables;
import jcl.types.DoubleFloat;
import jcl.types.Float;
import jcl.types.LongFloat;
import jcl.types.ShortFloat;
import jcl.types.SingleFloat;

import java.math.BigDecimal;

public abstract class FloatPrinter<O> implements LispPrinter<O> {

	private static final long serialVersionUID = -2482654535846962538L;

	@Override
	public String print(final O object) {
		final jcl.types.Float floatFormat = getFloatType(object);
		final Float defaultFloatFormat = ReaderVariables.READ_DEFAULT_FLOAT_FORMAT.getValue();

		final BigDecimal bigDecimal = getBigDecimal(object);
		String bigDecimalString = bigDecimal.toString();
		if (!floatFormat.equals(defaultFloatFormat)) {
			if (floatFormat.equals(ShortFloat.INSTANCE)) {
				bigDecimalString = bigDecimalString.replace('E', 'S');
			} else if (floatFormat.equals(SingleFloat.INSTANCE)) {
				bigDecimalString = bigDecimalString.replace('E', 'F');
			} else if (floatFormat.equals(DoubleFloat.INSTANCE)) {
				bigDecimalString = bigDecimalString.replace('E', 'D');
			} else if (floatFormat.equals(LongFloat.INSTANCE)) {
				bigDecimalString = bigDecimalString.replace('E', 'L');
			}
		}

		return bigDecimalString;
	}

	protected abstract jcl.types.Float getFloatType(O object);

	protected abstract BigDecimal getBigDecimal(O object);
}
