/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Objects;

import jcl.util.NumberUtils;
import org.apache.commons.math3.fraction.BigFraction;

/**
 * This class represents an interval designator, found within compound type specifiers.
 *
 * @param <N>
 * 		the number type used for the interval boundaries
 */
public class IntervalDesignator<N extends Number> {

	/**
	 * The lower boundary for the interval. This is 'inclusive'. The 'inclusive/exclusive' handling is taken care of
	 * before creation.
	 */
	private final N lowerBound;

	/**
	 * The upper boundary for the interval.  This is 'inclusive'. The 'inclusive/exclusive' handling is taken care of
	 * before creation.
	 */
	private final N upperBound;

	/**
	 * Constructor for creating an interval for the provided lower and upper boundaries.
	 *
	 * @param lowerBound
	 * 		the lower boundary
	 * @param upperBound
	 * 		the upper boundary
	 */
	public IntervalDesignator(final N lowerBound, final N upperBound) {
		if ((lowerBound != null) && (upperBound != null) && (lowerBound.getClass() != upperBound.getClass())) {
			throw new IllegalArgumentException("Both upper and lower bounds must be of the same class type.");
		}

		this.lowerBound = lowerBound;
		this.upperBound = upperBound;
	}

	/**
	 * Getter for the lower bound value.
	 *
	 * @return the lower bound value
	 */
	public N getLowerBound() {
		return lowerBound;
	}

	/**
	 * Getter for the upper bound value.
	 *
	 * @return the upper bound value
	 */
	public N getUpperBound() {
		return upperBound;
	}

	@Override
	public int hashCode() {
		return Objects.hash(lowerBound, upperBound);
	}

	@Override
	public boolean equals(final Object obj) {
		if (this == obj) {
			return true;
		}

		if ((obj == null) || (getClass() != obj.getClass())) {
			return false;
		}

		final IntervalDesignator<?> intervalDesignator = (IntervalDesignator<?>) obj;

		return isWithinBounds(intervalDesignator.lowerBound) && isWithinBounds(intervalDesignator.upperBound);
	}

	/**
	 * This method checks to see whether the number provided is within the bound of this interval.
	 *
	 * @param number
	 * 		the number to verify is within bounds
	 *
	 * @return true if the number is within the interval; false otherwise
	 */
	private boolean isWithinBounds(final Number number) {
		final int lowerCompare = numberCompareTo(lowerBound, number);
		final int upperCompare = numberCompareTo(upperBound, number);

		return (lowerCompare >= 0) && (upperCompare <= 0);
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
	private static int numberCompareTo(final Number num1, final Number num2) {
		if ((num1 == null) && (num2 == null)) {
			return 0;
		} else if ((num1 instanceof BigInteger) && (num2 instanceof BigInteger)) {
			return ((BigInteger) num1).compareTo((BigInteger) num2);
		} else if ((num1 instanceof BigInteger) && (num2 instanceof BigDecimal)) {
			return NumberUtils.bigDecimalValue((BigInteger) num1).compareTo((BigDecimal) num2);
		} else if ((num1 instanceof BigInteger) && (num2 instanceof BigFraction)) {
			return new BigFraction((BigInteger) num1).compareTo((BigFraction) num2);
		} else if ((num1 instanceof BigDecimal) && (num2 instanceof BigInteger)) {
			return ((BigDecimal) num1).compareTo(NumberUtils.bigDecimalValue((BigInteger) num2));
		} else if ((num1 instanceof BigDecimal) && (num2 instanceof BigDecimal)) {
			return ((BigDecimal) num1).compareTo((BigDecimal) num2);
		} else if ((num1 instanceof BigDecimal) && (num2 instanceof BigFraction)) {
			return ((BigDecimal) num1).compareTo(NumberUtils.bigDecimalValue((BigFraction) num2));
		} else if ((num1 instanceof BigFraction) && (num2 instanceof BigInteger)) {
			return ((BigFraction) num1).compareTo(new BigFraction((BigInteger) num2));
		} else if ((num1 instanceof BigFraction) && (num2 instanceof BigDecimal)) {
			return NumberUtils.bigDecimalValue((BigFraction) num1).compareTo((BigDecimal) num2);
		} else if ((num1 instanceof BigFraction) && (num2 instanceof BigFraction)) {
			return ((BigFraction) num1).compareTo((BigFraction) num2);
		} else {
			return -1;
		}
	}

	@Override
	public String toString() {
		return ((lowerBound == null) ? "*" : lowerBound) + " " + ((upperBound == null) ? "*" : upperBound);
	}
}
