package jcl.lang;

import java.util.List;

import jcl.lang.condition.exception.ErrorException;
import jcl.lang.internal.ComplexArrayStructImpl;
import jcl.lang.internal.NILArrayStructImpl;
import jcl.lang.internal.SimpleArrayStructImpl;
import jcl.lang.statics.CommonLispSymbols;

/**
 * The {@link ArrayStruct} is the object representation of a Lisp 'array' type.
 */
public interface ArrayStruct extends LispStruct {

	/**
	 * Determines if this ArrayStruct is adjustable.
	 *
	 * @return {@code true} if the ArrayStruct is adjustable; {@code false} otherwise
	 */
	default BooleanStruct adjustableArrayP() {
		return NILStruct.INSTANCE;
	}

	/**
	 * Retrieves the {@link LispStruct} at the provided position determined by the provided {@code subscripts}.
	 *
	 * @param subscripts
	 * 		the position of the {@link LispStruct} to retrieve
	 *
	 * @return the {@link LispStruct} at the provided subscripts
	 */
	LispStruct aref(final IntegerStruct... subscripts);

	/**
	 * Sets the {@link LispStruct} at the provided position determined by the provided {@code subscripts} to the
	 * provided new {@link LispStruct} element.
	 *
	 * @param newElement
	 * 		the new {@link LispStruct} to set
	 * @param subscripts
	 * 		the position to modify and set as the provided new element value
	 *
	 * @return the new {@link LispStruct} element
	 */
	LispStruct setfAref(final LispStruct newElement, final IntegerStruct... subscripts);

	/**
	 * Returns the axis-number dimension of array. (Any fill pointer is ignored.)
	 *
	 * @param axisNumber
	 * 		an integer greater than or equal to zero and less than the rank of the array
	 *
	 * @return the axis-number dimension of array
	 */
	IntegerStruct arrayDimension(final IntegerStruct axisNumber);

	/**
	 * Returns a list of the dimensions of array. (If array is a vector with a fill pointer, that fill pointer is
	 * ignored.)
	 *
	 * @return a list of the dimensions of array
	 */
	ListStruct arrayDimensions();

	/**
	 * Returns a type specifier which represents the actual array element type of the array, which is the set of objects
	 * that such an array can hold. (Because of array upgrading, this type specifier can in some cases denote a
	 * supertype of the expressed array element type of the array.)
	 *
	 * @return a type specifier which represents the actual array element type of the array
	 */
	LispStruct arrayElementType();

	/**
	 * Returns true if array has a fill pointer; otherwise returns false.
	 *
	 * @return true if array has a fill pointer; otherwise returns false
	 */
	default BooleanStruct arrayHasFillPointerP() {
		return NILStruct.INSTANCE;
	}

	/**
	 * If the array is a displaced array, returns the values of the :displaced-to and :displaced-index-offset options
	 * for the array. If the array is not a displaced array, nil and 0 are returned.
	 *
	 * @return the values of the :displaced-to and :displaced-index-offset options, or nil and 0
	 */
	default ArrayDisplacement arrayDisplacement() {
		return ArrayDisplacement.DEFAULT;
	}

	/**
	 * Returns true if the subscripts are all in bounds for array; otherwise returns false. (If array is a vector with a
	 * fill pointer, that fill pointer is ignored.)
	 *
	 * @param subscripts
	 * 		a list of integers of length equal to the rank of the array
	 *
	 * @return true if the subscripts are all in bounds for array; otherwise returns false
	 */
	default BooleanStruct arrayInBoundsP(final IntegerStruct... subscripts) {
		try {
			arrayRowMajorIndex(subscripts);
			return TStruct.INSTANCE;
		} catch (final ErrorException ignored) {
			return NILStruct.INSTANCE;
		}
	}

	/**
	 * Returns the number of dimensions of array.
	 *
	 * @return the number of dimensions of array
	 */
	IntegerStruct arrayRank();

	/**
	 * Computes the position according to the row-major ordering of array for the element that is specified by
	 * subscripts, and returns the offset of the element in the computed position from the beginning of array.
	 * <p>
	 * For a one-dimensional array, the result of array-row-major-index equals subscript.
	 * <p>
	 * array-row-major-index ignores fill pointers.
	 *
	 * @param subscripts
	 * 		a list of valid array indices for the array
	 *
	 * @return the position according to the row-major ordering of array
	 */
	IntegerStruct arrayRowMajorIndex(final IntegerStruct... subscripts);

	/**
	 * Returns the array total size of the array.
	 *
	 * @return the array total size of the array
	 */
	IntegerStruct arrayTotalSize();

	/**
	 * Considers array as a vector by viewing its elements in row-major order, and returns the element of that vector
	 * which is referred to by the given index.
	 *
	 * @param index
	 * 		the row-major index used to retrieve the element
	 *
	 * @return the element in row-major order which corresponds to the provided index
	 */
	LispStruct rowMajorAref(final IntegerStruct index);

	/**
	 * Considers array as a vector by viewing its elements in row-major order, and sets the element of that vector which
	 * is referred to by the given index to the new element value provided.
	 *
	 * @param newElement
	 * 		the element to be set
	 * @param index
	 * 		the row-major index used to set the element
	 *
	 * @return the newly set element
	 */
	LispStruct setfRowMajorAref(final LispStruct newElement, final IntegerStruct index);

	/**
	 * Returns the element type of the most specialized array representation capable of holding items of the type
	 * denoted by typespec.
	 *
	 * @param type
	 * 		the typespec used to determine the appropriate upgraded array element-type
	 *
	 * @return the element type of the most specialized array representation
	 */
	static LispStruct upgradedArrayElementType(final LispStruct type) {
		if (CommonLispSymbols.CHARACTER.eq(type)
				|| CommonLispSymbols.BASE_CHAR.eq(type)
				|| CommonLispSymbols.STANDARD_CHAR.eq(type)
				|| CommonLispSymbols.EXTENDED_CHAR.eq(type)) {
			return CommonLispSymbols.CHARACTER;
		}
		if (CommonLispSymbols.BIT.eq(type)) {
			return CommonLispSymbols.BIT;
		}
		if (CommonLispSymbols.NULL.eq(type)) {
			return CommonLispSymbols.NIL;
		}
		if (CommonLispSymbols.NIL.eq(type)) {
			return CommonLispSymbols.NIL;
		}
		return CommonLispSymbols.T;
	}

	/**
	 * Returns a new ArrayStruct representation of the provided element-type, content, and adjustable.
	 *
	 * @param elementType
	 * 		the array elementType
	 * @param content
	 * 		the content of the array
	 * @param adjustable
	 * 		whether or not the array is adjustable
	 *
	 * @return a new ArrayStruct representation of the provided element-type, content, and adjustable
	 */
	static ArrayStruct toLispArray(final SymbolStruct elementType, final LispStruct content,
	                               final BooleanStruct adjustable) {
		final BooleanStruct realAdjustable = (adjustable == null) ? NILStruct.INSTANCE : adjustable;
		return new NILArrayStructImpl(elementType, content, realAdjustable);
	}

	/**
	 * Returns a new ArrayStruct representation of the provided dimensions, element-type, and contents.
	 *
	 * @param dimensions
	 * 		the array dimensions
	 * @param elementType
	 * 		the array elementType
	 * @param contents
	 * 		the array contents
	 *
	 * @return a new ArrayStruct representation of the provided dimensions, element-type, and contents
	 */
	static ArrayStruct toLispArray(final List<IntegerStruct> dimensions, final SymbolStruct elementType,
	                               final List<LispStruct> contents) {
		return new SimpleArrayStructImpl(dimensions, elementType, contents);
	}

	/**
	 * Returns a new ArrayStruct representation of the provided dimensions, element-type, contents, and adjustable.
	 *
	 * @param dimensions
	 * 		the array dimensions
	 * @param elementType
	 * 		the array elementType
	 * @param contents
	 * 		the array contents
	 * @param adjustable
	 * 		whether or not the array is adjustable
	 *
	 * @return a new ArrayStruct representation of the provided dimensions, element-type, contents, and adjustable
	 */
	static ArrayStruct toLispArray(final List<IntegerStruct> dimensions, final SymbolStruct elementType,
	                               final List<LispStruct> contents, final BooleanStruct adjustable) {
		final BooleanStruct realAdjustable = (adjustable == null) ? NILStruct.INSTANCE : adjustable;
		return new ComplexArrayStructImpl(dimensions, elementType, contents, realAdjustable);
	}

	/**
	 * Returns a new ArrayStruct representation of the provided dimensions, element-type, displacedTo,
	 * displacedIndexOffset, and adjustable.
	 *
	 * @param dimensions
	 * 		the array dimensions
	 * @param elementType
	 * 		the array elementType
	 * @param displacedTo
	 * 		the array structure that this array structure will be displaced to for content values
	 * @param displacedIndexOffset
	 * 		the offset of the index lookup for the content value into the displaced array structure
	 * @param adjustable
	 * 		whether or not the array is adjustable
	 *
	 * @return a new ArrayStruct representation of the provided dimensions, element-type, displacedTo,
	 * displacedIndexOffset, and adjustable
	 */
	static ArrayStruct toLispArray(final List<IntegerStruct> dimensions, final SymbolStruct elementType,
	                               final ArrayStruct displacedTo, final IntegerStruct displacedIndexOffset,
	                               final BooleanStruct adjustable) {
		final BooleanStruct realAdjustable = (adjustable == null) ? NILStruct.INSTANCE : adjustable;
		return new ComplexArrayStructImpl(dimensions, elementType, displacedTo, displacedIndexOffset, realAdjustable);
	}

	/*
	LISP-STRUCT
	 */

	@Override
	default boolean equalp(final LispStruct object) {
		if (eq(object)) {
			return true;
		}
		if (object instanceof final ArrayStruct a) {
			if (!arrayRank().eql(a.arrayRank())) {
				return false;
			}
			for (int i = 0; i < arrayRank().toJavaInt(); i++) {
				final IntegerStruct axisNumber = IntegerStruct.toLispInteger(i);
				if (!arrayDimension(axisNumber).eql(a.arrayDimension(axisNumber))) {
					return false;
				}
			}
			for (int i = 0; i < arrayTotalSize().toJavaInt(); i++) {
				final IntegerStruct index = IntegerStruct.toLispInteger(i);
				if (!rowMajorAref(index).equalp(a.rowMajorAref(index))) {
					return false;
				}
			}
			return true;
		}
		return false;
	}
}
