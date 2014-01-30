package jcl.types.util;

import jcl.types.LispType;
import jcl.types.arrays.Array;
import jcl.types.typespecifiers.designator.DimensionsDesignator;
import org.apache.commons.math3.fraction.BigFraction;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * A Utility class for handling common utilities for lisp types.
 */
public final class TypeUtils {

	/**
	 * Private constructor.
	 */
	private TypeUtils() {
	}

	/**
	 * This method tests to see if two {@code Array}s are equivalent.
	 *
	 * @param array1 the first type to test
	 * @param array2 the second type to test
	 * @return true if the types are equivalent; false otherwise
	 */
	public static boolean isArrayLispTypeEqual(final Array array1, final Array array2) {

		final DimensionsDesignator dimensions1 = array1.getDimensions();
		if (dimensions1 != null) {
			final DimensionsDesignator dimensions2 = array2.getDimensions();
			if (!dimensions1.equals(dimensions2)) {
				return false;
			}
		}

		final LispType elementType1 = array1.getElementType();
		if (elementType1 != null) {
			final LispType elementType2 = array2.getElementType();
			if (!elementType1.equals(elementType2)) {
				return false;
			}
		}

		return true;
	}


	/**
	 * This method compares the two provided numbers to test their equality.
	 * NOTE: This only will compare BigIntegers, BigDecimals, and BigFractions!!
	 *
	 * @param num1 the first number to compare
	 * @param num2 the second number to compare
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
