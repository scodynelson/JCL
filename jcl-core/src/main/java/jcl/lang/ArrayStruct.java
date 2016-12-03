package jcl.lang;

import java.util.List;

import jcl.type.LispType;

/**
 * The {@link ArrayStruct} is the object representation of a Lisp 'array' type.
 *
 * @param <TYPE>
 * 		the type of the array contents
 */
public interface ArrayStruct<TYPE extends LispStruct> extends LispStruct {

	ArrayStruct<TYPE> adjustArray(final List<IntegerStruct> dimensions, final LispType elementType,
	                              final TYPE initialElement, final BooleanStruct isAdjustable);

	ArrayStruct<TYPE> adjustArray(final List<IntegerStruct> dimensions, final LispType elementType,
	                              final SequenceStruct initialContents, final BooleanStruct isAdjustable);

	ArrayStruct<TYPE> adjustArray(final List<IntegerStruct> dimensions, final LispType elementType,
	                              final ArrayStruct<TYPE> displacedTo, final IntegerStruct displacedIndexOffset,
	                              final BooleanStruct isAdjustable);

	ArrayStruct<TYPE> adjustArray(final List<IntegerStruct> newDimensions, final LispType elementType,
	                              final TYPE initialElement);

	ArrayStruct<TYPE> adjustArray(final List<IntegerStruct> newDimensions, final LispType elementType,
	                              final SequenceStruct initialContents);

	BooleanStruct adjustableArrayP();

	TYPE aref(final IntegerStruct... subscripts);

	TYPE setfAref(final TYPE newElement, final IntegerStruct... subscripts);

	IntegerStruct arrayDimension(final IntegerStruct axisNumber);

	ListStruct arrayDimensions();

	/**
	 * Gets the array elementType.
	 *
	 * @return array elementType
	 */
	LispType arrayElementType();

	BooleanStruct arrayHasFillPointerP();

	ValuesStruct arrayDisplacement();

	BooleanStruct arrayInBoundsP(final IntegerStruct... subscripts);

	/**
	 * Gets the array rank.
	 *
	 * @return array rank
	 */
	IntegerStruct arrayRank();

	IntegerStruct arrayRowMajorIndex(final IntegerStruct... subscripts);

	/**
	 * Gets the array's total size.
	 *
	 * @return array's total size
	 */
	IntegerStruct arrayTotalSize();

	TYPE rowMajorAref(final IntegerStruct index);

	// =================

	/**
	 * Gets the array contents.
	 *
	 * @return array contents
	 */
	List<TYPE> getContents();

	/**
	 * Gets the array dimensions.
	 *
	 * @return array dimensions
	 */
	List<Integer> getDimensions();
}
