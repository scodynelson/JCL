/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl.struct;

import jcl.numbers.IntegerStruct;
import jcl.numbers.RatioStruct;
import jcl.printer.impl.RatioPrinter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class RatioStructPrinter extends RatioPrinter<RatioStruct, IntegerStruct> {

	private static final long serialVersionUID = -491452763122265968L;

	@Autowired
	private IntegerStructPrinter integerStructPrinter;

	@Override
	protected IntegerStruct getNumerator(final RatioStruct object) {
		return new IntegerStruct(object.getBigFraction().getNumerator());
	}

	@Override
	protected IntegerStruct getDenominator(final RatioStruct object) {
		return new IntegerStruct(object.getBigFraction().getDenominator());
	}

	@Override
	protected IntegerStructPrinter getIntegerPrinter() {
		return integerStructPrinter;
	}
}
