/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl.struct;

import jcl.numbers.IntegerStruct;
import jcl.printer.impl.IntegerPrinter;
import org.springframework.stereotype.Component;

import java.math.BigInteger;

@Component
public class IntegerStructPrinter extends IntegerPrinter<IntegerStruct> {

	private static final long serialVersionUID = 3499223261380244866L;

	@Override
	protected BigInteger getBigInteger(final IntegerStruct object) {
		return object.getBigInteger();
	}
}
