/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl.struct;

import jcl.numbers.FloatStruct;
import jcl.printer.impl.FloatPrinter;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;

@Component
public class FloatStructPrinter extends FloatPrinter<FloatStruct> {

	private static final long serialVersionUID = 5762270611242790794L;

	@Override
	protected jcl.types.Float getFloatType(final FloatStruct object) {
		return (jcl.types.Float) object.getType();
	}

	@Override
	protected BigDecimal getBigDecimal(final FloatStruct object) {
		return object.getBigDecimal();
	}
}
