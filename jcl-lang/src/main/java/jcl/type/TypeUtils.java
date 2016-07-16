/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

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
	 * This method tests to see if two {@link ArrayType}s are equivalent.
	 *
	 * @param arrayType1
	 * 		the first {@link ArrayType} to test
	 * @param arrayType2
	 * 		the second {@link ArrayType} to test
	 *
	 * @return true if the {@link ArrayType}s are equivalent; false otherwise
	 */
	static boolean isArrayTypeEqual(final ArrayType arrayType1, final ArrayType arrayType2) {

		final DimensionsDesignator dimensions1 = arrayType1.getDimensions();
		if (dimensions1 != null) {
			final DimensionsDesignator dimensions2 = arrayType2.getDimensions();
			if (!dimensions1.equals(dimensions2)) {
				return false;
			}
		}

		final LispType elementType1 = arrayType1.getElementType();
		if (elementType1 != null) {
			final LispType elementType2 = arrayType2.getElementType();
			if (!elementType1.equals(elementType2)) {
				return false;
			}
		}

		return true;
	}
}
