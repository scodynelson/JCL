/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types.typespecifiers;

import org.apache.commons.math3.fraction.BigFraction;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * A Utility class for handling common utilities for lisp type specifiers.
 */
public final class TypeSpecifierUtils {

	/**
	 * Private constructor.
	 */
	private TypeSpecifierUtils() {
	}

	/**
	 * This method compares the two provided numbers to test their equality.
	 * NOTE: This only will compare BigIntegers, BigDecimals, and BigFractions!!
	 *
	 * @param num1
	 * 		the first number to compare
	 * @param num2
	 * 		the second number to compare
	 *
	 * @return the comparison of the two numbers
	 */
	public static int numberCompareTo(final Number num1, final Number num2) {
		if ((num1 == null) && (num2 == null)) {
			return 0;
		} else if ((num1 instanceof BigInteger) && (num2 instanceof BigInteger)) {
			return ((BigInteger) num1).compareTo((BigInteger) num2);
		} else if ((num1 instanceof BigInteger) && (num2 instanceof BigDecimal)) {
			return new BigDecimal((BigInteger) num1).compareTo((BigDecimal) num2);
		} else if ((num1 instanceof BigInteger) && (num2 instanceof BigFraction)) {
			return new BigFraction((BigInteger) num1).compareTo((BigFraction) num2);
		} else if ((num1 instanceof BigDecimal) && (num2 instanceof BigInteger)) {
			return ((BigDecimal) num1).compareTo(new BigDecimal((BigInteger) num2));
		} else if ((num1 instanceof BigDecimal) && (num2 instanceof BigDecimal)) {
			return ((BigDecimal) num1).compareTo((BigDecimal) num2);
		} else if ((num1 instanceof BigDecimal) && (num2 instanceof BigFraction)) {
			return ((BigDecimal) num1).compareTo(((BigFraction) num2).bigDecimalValue());
		} else if ((num1 instanceof BigFraction) && (num2 instanceof BigInteger)) {
			return ((BigFraction) num1).compareTo(new BigFraction((BigInteger) num2));
		} else if ((num1 instanceof BigFraction) && (num2 instanceof BigDecimal)) {
			return ((BigFraction) num1).bigDecimalValue().compareTo((BigDecimal) num2);
		} else if ((num1 instanceof BigFraction) && (num2 instanceof BigFraction)) {
			return ((BigFraction) num1).compareTo((BigFraction) num2);
		} else {
			return -1;
		}
	}
}
