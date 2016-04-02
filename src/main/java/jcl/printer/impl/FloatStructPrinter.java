/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

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

	@Override
	public String print(final FloatStruct object) {
		final FloatType floatType = (FloatType) object.getType();
		final FloatType defaultFloatFormat = ReaderVariables.READ_DEFAULT_FLOAT_FORMAT.getVariableValue();

		String floatString = object.toString();
		if (!floatType.equals(defaultFloatFormat)) {
			if (floatType.equals(ShortFloatType.INSTANCE)) {
				floatString = floatString.replace('E', 'S');
			} else if (floatType.equals(SingleFloatType.INSTANCE)) {
				floatString = floatString.replace('E', 'F');
			} else if (floatType.equals(DoubleFloatType.INSTANCE)) {
				floatString = floatString.replace('E', 'D');
			} else if (floatType.equals(LongFloatType.INSTANCE)) {
				floatString = floatString.replace('E', 'L');
			}
		}

		return floatString;
	}
}
