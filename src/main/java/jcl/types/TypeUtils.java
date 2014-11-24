package jcl.types;

import jcl.LispType;
import jcl.types.typespecifiers.designator.DimensionsDesignator;

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
	 * This method tests to see if two {@link Array}s are equivalent.
	 *
	 * @param array1
	 * 		the first type to test
	 * @param array2
	 * 		the second type to test
	 *
	 * @return true if the types are equivalent; false otherwise
	 */
	static boolean isArrayLispTypeEqual(final Array array1, final Array array2) {

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
}
