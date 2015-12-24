/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import java.math.BigDecimal;

import jcl.numbers.FloatStruct;
import jcl.printer.LispPrinter;
import jcl.reader.struct.ReaderVariables;
import jcl.types.DoubleFloatType;
import jcl.types.FloatType;
import jcl.types.LongFloatType;
import jcl.types.ShortFloatType;
import jcl.types.SingleFloatType;
import org.springframework.stereotype.Component;

@Component
public class FloatStructPrinter implements LispPrinter<FloatStruct> {

	private static final long serialVersionUID = 5762270611242790794L;

	@Override
	public String print(final FloatStruct object) {
		final FloatType floatType = (FloatType) object.getType();
		final FloatType defaultFloatFormat = ReaderVariables.READ_DEFAULT_FLOAT_FORMAT.getVariableValue();

		final BigDecimal bigDecimal = object.getBigDecimal();
		String bigDecimalString = bigDecimal.toString();
		if (!floatType.equals(defaultFloatFormat)) {
			if (floatType.equals(ShortFloatType.INSTANCE)) {
				bigDecimalString = bigDecimalString.replace('E', 'S');
			} else if (floatType.equals(SingleFloatType.INSTANCE)) {
				bigDecimalString = bigDecimalString.replace('E', 'F');
			} else if (floatType.equals(DoubleFloatType.INSTANCE)) {
				bigDecimalString = bigDecimalString.replace('E', 'D');
			} else if (floatType.equals(LongFloatType.INSTANCE)) {
				bigDecimalString = bigDecimalString.replace('E', 'L');
			}
		}

		return bigDecimalString;
	}
}
