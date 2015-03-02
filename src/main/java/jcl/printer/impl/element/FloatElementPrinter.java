/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl.element;

import jcl.compiler.real.element.FloatElement;
import jcl.printer.impl.FloatPrinter;
import org.springframework.stereotype.Component;

import java.math.BigDecimal;

@Component
public class FloatElementPrinter extends FloatPrinter<FloatElement> {

	private static final long serialVersionUID = -4957809224621379673L;

	@Override
	protected jcl.types.Float getFloatType(final FloatElement object) {
		return object.getFloatFormat();
	}

	@Override
	protected BigDecimal getBigDecimal(final FloatElement object) {
		return object.getBigDecimal();
	}
}
