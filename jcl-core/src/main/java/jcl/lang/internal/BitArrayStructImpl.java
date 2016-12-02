package jcl.lang.internal;

import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jcl.lang.BitArrayStruct;
import jcl.lang.IntegerStruct;
import jcl.type.ArrayType;
import jcl.type.BitType;
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
	 * @param contents
	 * 		the array contents
	 * @param isAdjustable
	 */
	protected BitArrayStructImpl(final ArrayType arrayType, final List<Integer> dimensions,
	                             final List<IntegerStruct> contents, final boolean isAdjustable) {
		super(arrayType, dimensions, BitType.INSTANCE, contents, isAdjustable);
	}

	/**
	 * Protected constructor.
	 *
	 * @param arrayType
	 * 		the array type
	 * @param dimensions
	 * 		the array dimensions
	 * @param isAdjustable
	 * 		whether or not the array is adjustable
	 */
	protected BitArrayStructImpl(final ArrayType arrayType, final List<Integer> dimensions, final boolean isAdjustable,
	                             final BitArrayStruct displacedTo, final Integer displacedIndexOffset) {
		super(arrayType, dimensions, BitType.INSTANCE, isAdjustable, displacedTo, displacedIndexOffset);
	}

	public static BitArrayStruct valueOfBA(final List<Integer> dimensions, final List<IntegerStruct> initialContents,
	                                       final boolean isAdjustable) {
		// Check input data
		// TODO: cleanup
		areContentsValidForDimensionsAndElementType(dimensions, BitType.INSTANCE, initialContents);

		final ArrayType arrayType = getArrayType(isAdjustable);
		return new BitArrayStructImpl(arrayType, dimensions, initialContents, isAdjustable);
	}

	public static BitArrayStruct valueOf(final List<Integer> dimensions, final boolean isAdjustable,
	                                     final BitArrayStruct displacedTo, final Integer displacedIndexOffset) {
		return new BitArrayStructImpl(ArrayType.INSTANCE, dimensions, isAdjustable, displacedTo, displacedIndexOffset);
	}

	public static BitArrayStruct valueOf(final List<Integer> dimensions, final IntegerStruct initialElement) {
		final int totalElements = dimensions.stream()
		                                    .mapToInt(Integer::intValue)
		                                    .sum();
		final List<IntegerStruct> initialContents = Stream.generate(() -> initialElement)
		                                                  .limit(totalElements)
		                                                  .collect(Collectors.toList());

		// Check input data
		// TODO: cleanup
		areContentsValidForDimensionsAndElementType(dimensions, BitType.INSTANCE, initialContents);

		return new BitArrayStructImpl(SimpleArrayType.INSTANCE, dimensions, initialContents, false);
	}

	public static BitArrayStruct valueOfBA(final List<Integer> dimensions, final List<IntegerStruct> initialContents) {

		// Check input data
		// TODO: cleanup
		areContentsValidForDimensionsAndElementType(dimensions, BitType.INSTANCE, initialContents);

		return new BitArrayStructImpl(SimpleArrayType.INSTANCE, dimensions, initialContents, false);
	}

	@Override
	public BitArrayStruct copyBitArray() {
		return new BitArrayStructImpl(getArrayType(isAdjustable), dimensions, contents, isAdjustable);
	}
}
