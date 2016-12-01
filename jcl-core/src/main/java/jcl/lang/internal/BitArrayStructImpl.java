package jcl.lang.internal;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jcl.lang.BitArrayStruct;
import jcl.lang.IntegerStruct;
import jcl.type.ArrayType;
import jcl.type.LispType;
import jcl.type.SimpleArrayType;

/**
 * The {@link BitArrayStructImpl} is the object representation of a Lisp 'bit-array' type.
 */
public class BitArrayStructImpl extends ArrayStructImpl<IntegerStruct> implements BitArrayStruct {

	/**
	 * Protected constructor.
	 *
	 * @param arrayType
	 * 		the array type
	 * @param dimensions
	 * 		the array dimensions
	 * @param elementType
	 * 		the array elementType
	 * @param contents
	 * 		the array contents
	 * @param isAdjustable
	 */
	protected BitArrayStructImpl(final ArrayType arrayType, final List<Integer> dimensions,
	                             final LispType elementType, final List<IntegerStruct> contents,
	                             final boolean isAdjustable) {
		super(arrayType, dimensions, elementType, contents, isAdjustable);
	}

	/**
	 * Protected constructor.
	 *
	 * @param arrayType
	 * 		the array type
	 * @param dimensions
	 * 		the array dimensions
	 * @param elementType
	 * 		the array elementType
	 * @param isAdjustable
	 * 		whether or not the array is adjustable
	 */
	protected BitArrayStructImpl(final ArrayType arrayType, final List<Integer> dimensions, final LispType elementType,
	                             final boolean isAdjustable, final BitArrayStruct displacedTo,
	                             final Integer displacedIndexOffset) {
		super(arrayType, dimensions, elementType, isAdjustable, displacedTo, displacedIndexOffset);
	}

	public static BitArrayStruct valueOfBA(final List<Integer> dimensions,
	                                       final LispType elementType,
	                                       final List<IntegerStruct> initialContents,
	                                       final boolean isAdjustable) {

		// Check input data
		areContentsValidForDimensionsAndElementType(dimensions, elementType, initialContents);

		final ArrayType arrayType = getArrayType(isAdjustable);
		return new BitArrayStructImpl(arrayType, dimensions, elementType, initialContents, isAdjustable);
	}

	public static BitArrayStruct valueOfBA(final List<Integer> dimensions,
	                                       final LispType elementType,
	                                       final boolean isAdjustable,
	                                       final BitArrayStruct displacedTo,
	                                       final Integer displacedIndexOffset) {
		return new BitArrayStructImpl(ArrayType.INSTANCE, dimensions, elementType, isAdjustable, displacedTo,
		                              displacedIndexOffset);
	}

	public static BitArrayStruct valueOfBA(final List<Integer> dimensions,
	                                       final LispType elementType,
	                                       final IntegerStruct initialElement) {
		final int totalElements = dimensions.stream()
		                                    .mapToInt(Integer::intValue)
		                                    .sum();
		final List<IntegerStruct> initialContents = Stream.generate(() -> initialElement)
		                                                  .limit(totalElements)
		                                                  .collect(Collectors.toList());

		// Check input data
		// TODO: is this needed?? Optimize...
		areContentsValidForDimensionsAndElementType(dimensions, elementType, initialContents);

		return new BitArrayStructImpl(SimpleArrayType.INSTANCE, dimensions, elementType, initialContents, false);
	}

	public static BitArrayStruct valueOfBA(final List<Integer> dimensions,
	                                       final LispType elementType,
	                                       final List<IntegerStruct> initialContents) {

		// Check input data
		areContentsValidForDimensionsAndElementType(dimensions, elementType, initialContents);

		return new BitArrayStructImpl(SimpleArrayType.INSTANCE, dimensions, elementType, initialContents, false);
	}
}
