package jcl.lang;

import java.util.List;

import jcl.type.BaseCharType;
import jcl.type.BitType;
import jcl.type.CharacterType;
import jcl.type.ExtendedCharType;
import jcl.type.LispType;
import jcl.type.NILType;
import jcl.type.StandardCharType;
import jcl.type.TType;

/**
 * The {@link ArrayStruct} is the object representation of a Lisp 'array' type.
 *
 * @param <TYPE>
 * 		the type of the array contents
 */
public interface ArrayStruct<TYPE extends LispStruct> extends LispStruct {

	ArrayStruct<TYPE> adjustArray(final List<IntegerStruct> dimensions, final LispType elementType,
	                              final TYPE initialElement, final IntegerStruct fillPointer);

	ArrayStruct<TYPE> adjustArray(final List<IntegerStruct> dimensions, final LispType elementType,
	                              final SequenceStruct initialContents, final IntegerStruct fillPointer);

	ArrayStruct<TYPE> adjustArray(final List<IntegerStruct> dimensions, final LispType elementType,
	                              final IntegerStruct fillPointer, final ArrayStruct<TYPE> displacedTo,
	                              final IntegerStruct displacedIndexOffset);

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

	TYPE setfRowMajorAref(final TYPE newElement, final IntegerStruct index);

	static LispType upgradedArrayElementType(final LispType type) {
		if (CharacterType.INSTANCE.equals(type)
				|| BaseCharType.INSTANCE.equals(type)
				|| StandardCharType.INSTANCE.equals(type)
				|| ExtendedCharType.INSTANCE.equals(type)) {
			return CharacterType.INSTANCE;
		}
		if (BitType.INSTANCE.equals(type)) {
			return BitType.INSTANCE;
		}
		if (NILType.INSTANCE.equals(type)) {
			return NILType.INSTANCE;
		}
		return TType.INSTANCE;
	}

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
