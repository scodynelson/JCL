/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl.element;

import jcl.compiler.real.element.IntegerElement;
import jcl.printer.impl.IntegerPrinter;
import org.springframework.stereotype.Component;

import java.math.BigInteger;

@Component
public class IntegerElementPrinter extends IntegerPrinter<IntegerElement> {

	private static final long serialVersionUID = 3399918652383328180L;

	@Override
	protected BigInteger getBigInteger(final IntegerElement object) {
		return object.getBigInteger();
	}
}
