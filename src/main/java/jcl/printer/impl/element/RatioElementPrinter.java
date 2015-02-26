/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl.element;

import jcl.compiler.real.element.IntegerElement;
import jcl.compiler.real.element.RatioElement;
import jcl.printer.impl.RatioPrinter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class RatioElementPrinter extends RatioPrinter<RatioElement, IntegerElement> {

	@Autowired
	private IntegerElementPrinter integerElementPrinter;

	@Override
	protected IntegerElement getNumerator(final RatioElement object) {
		return new IntegerElement(object.getBigFraction().getNumerator());
	}

	@Override
	protected IntegerElement getDenominator(final RatioElement object) {
		return new IntegerElement(object.getBigFraction().getDenominator());
	}

	@Override
	protected IntegerElementPrinter getIntegerPrinter() {
		return integerElementPrinter;
	}
}
