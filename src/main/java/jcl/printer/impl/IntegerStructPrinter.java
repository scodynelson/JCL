/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.printer.impl;

import java.math.BigInteger;

import jcl.numbers.IntegerStruct;
import jcl.printer.LispPrinter;
import jcl.printer.PrinterVariables;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.springframework.stereotype.Component;

@Component
public class IntegerStructPrinter implements LispPrinter<IntegerStruct> {

	private static final long serialVersionUID = 3499223261380244866L;

	/**
	 * Int constant for the value '2'.
	 */
	private static final int TWO = 2;

	/**
	 * Int constant for the value '8'.
	 */
	private static final int EIGHT = 8;

	/**
	 * Int constant for the value '10'.
	 */
	private static final int TEN = 10;

	/**
	 * Int constant for the value '16'.
	 */
	private static final int SIXTEEN = 16;

	@Override
	public String print(final IntegerStruct object) {
		final boolean printRadix = PrinterVariables.PRINT_RADIX.getValue().booleanValue();
		final int printBase = PrinterVariables.PRINT_BASE.getValue().getBigInteger().intValue();

		final StringBuilder stringBuilder = new StringBuilder();

		if (printRadix) {
			if (printBase == TWO) {
				stringBuilder.append("#b");
			} else if (printBase == EIGHT) {
				stringBuilder.append("#o");
			} else if (printBase == SIXTEEN) {
				stringBuilder.append("#x");
			} else {
				stringBuilder.append('#');
				stringBuilder.append(printBase);
				stringBuilder.append('r');
			}
		}

		final BigInteger bigInteger = object.getBigInteger();
		stringBuilder.append(bigInteger.toString(printBase));

		if (printRadix && (printBase == TEN)) {
			stringBuilder.append('.');
		}

		return stringBuilder.toString();
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public int hashCode() {
		return HashCodeBuilder.reflectionHashCode(this);
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public boolean equals(final Object obj) {
		return EqualsBuilder.reflectionEquals(this, obj);
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
