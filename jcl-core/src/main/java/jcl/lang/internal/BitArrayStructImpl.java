package jcl.lang.internal;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jcl.lang.BitArrayStruct;
import jcl.lang.BooleanStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.SequenceStruct;
import jcl.lang.condition.exception.SimpleErrorException;
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
	BitArrayStructImpl(final ArrayType arrayType, final List<Integer> dimensions,
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
	BitArrayStructImpl(final ArrayType arrayType, final List<Integer> dimensions,
	                   final BitArrayStruct displacedTo, final Integer displacedIndexOffset,
	                   final boolean isAdjustable) {
		super(arrayType, dimensions, BitType.INSTANCE, displacedTo, displacedIndexOffset, isAdjustable);
	}

	public static BitArrayStruct valueOf(final List<IntegerStruct> dimensions, final IntegerStruct initialElement,
	                                     final BooleanStruct isAdjustable) {
		final List<Integer> dimensionInts = dimensions.stream()
		                                              .map(IntegerStruct::intValue)
		                                              .collect(Collectors.toList());
		final int totalSize = dimensionInts.stream()
		                                   .mapToInt(Integer::intValue)
		                                   .sum();
		final List<IntegerStruct> initialContents = Stream.generate(() -> initialElement)
		                                                  .limit(totalSize)
		                                                  .collect(Collectors.toList());

		final boolean adjustableBoolean = isAdjustable.booleanValue();
		final ArrayType arrayType = getArrayType(adjustableBoolean);
		return new BitArrayStructImpl(arrayType, dimensionInts, initialContents, adjustableBoolean);
	}

	public static BitArrayStruct valueOf(final List<IntegerStruct> dimensions, final SequenceStruct initialContents,
	                                     final BooleanStruct isAdjustable) {
		final List<Integer> dimensionInts = dimensions.stream()
		                                              .map(IntegerStruct::intValue)
		                                              .collect(Collectors.toList());
		final List<IntegerStruct> validContents = getValidContents(dimensionInts, initialContents);

		final boolean adjustableBoolean = isAdjustable.booleanValue();
		final ArrayType arrayType = getArrayType(adjustableBoolean);
		return new BitArrayStructImpl(arrayType, dimensionInts, validContents, adjustableBoolean);
	}

	public static BitArrayStruct valueOf(final List<IntegerStruct> dimensions, final BitArrayStruct displacedTo,
	                                     final IntegerStruct displacedIndexOffset, final BooleanStruct isAdjustable) {
		final List<Integer> dimensionInts = dimensions.stream()
		                                              .map(IntegerStruct::intValue)
		                                              .collect(Collectors.toList());

		// TODO: Total size of A be no smaller than the sum of the total size of B plus the offset 'n' supplied by the offset

		return new BitArrayStructImpl(ArrayType.INSTANCE, dimensionInts, displacedTo, displacedIndexOffset.intValue(),
		                              isAdjustable.booleanValue());
	}

	public static BitArrayStruct valueOf(final List<IntegerStruct> dimensions, final IntegerStruct initialElement) {
		final List<Integer> dimensionInts = dimensions.stream()
		                                              .map(IntegerStruct::intValue)
		                                              .collect(Collectors.toList());
		final int totalSize = dimensionInts.stream()
		                                   .mapToInt(Integer::intValue)
		                                   .sum();
		final List<IntegerStruct> initialContents = Stream.generate(() -> initialElement)
		                                                  .limit(totalSize)
		                                                  .collect(Collectors.toList());

		return new BitArrayStructImpl(SimpleArrayType.INSTANCE, dimensionInts, initialContents, false);
	}

	public static BitArrayStruct valueOf(final List<IntegerStruct> dimensions, final SequenceStruct initialContents) {
		final List<Integer> dimensionInts = dimensions.stream()
		                                              .map(IntegerStruct::intValue)
		                                              .collect(Collectors.toList());
		final List<IntegerStruct> validContents = getValidContents(dimensionInts, initialContents);

		return new BitArrayStructImpl(SimpleArrayType.INSTANCE, dimensionInts, validContents, false);
	}

	/**
	 * Determines if the provided {@code dimensionsToCheck} and {@code elementTypeToCheck} are valid for the provided
	 * {@code contentsToCheck}.
	 *
	 * @param dimensions
	 * 		the array dimensions to check
	 * @param initialContents
	 * 		the array contents to check
	 */
	private static List<IntegerStruct> getValidContents(final List<Integer> dimensions,
	                                                    final SequenceStruct initialContents) {
		final int numberOfDimensions = dimensions.size();
		if (numberOfDimensions == 0) {
			return Collections.emptyList();
		}

		if (numberOfDimensions == 1) {
			final int dimension = dimensions.get(0);
			if (initialContents.length() == dimension) {
				return initialContents.stream()
				                      .map(e -> (IntegerStruct) e)
				                      .collect(Collectors.toList());
			} else {
				throw new SimpleErrorException(
						initialContents + " doesn't match array dimensions of #<" + BitType.INSTANCE + ' ' + dimension + ">.");
			}
		}

		final List<IntegerStruct> validContents = new ArrayList<>();

		final int dimension = dimensions.get(0);
		if (initialContents.length() == dimension) {
			final List<Integer> subDimension = dimensions.subList(1, numberOfDimensions);

			for (final LispStruct contentToCheck : initialContents) {
				if (!(contentToCheck instanceof SequenceStruct)) {
					throw new SimpleErrorException(
							initialContents + " doesn't match array dimensions of #<" + BitType.INSTANCE + ' ' + dimension + ">.");
				}

				final SequenceStruct subContents = (SequenceStruct) contentToCheck;
				final List<IntegerStruct> validSubContents = getValidContents(subDimension, subContents);
				validContents.addAll(validSubContents);
			}
		} else {
			throw new SimpleErrorException(
					initialContents + " doesn't match array dimensions of #<" + BitType.INSTANCE + ' ' + dimension + ">.");
		}

		return validContents;
	}

	@Override
	public BitArrayStruct copyBitArray() {
		return new BitArrayStructImpl(getArrayType(isAdjustable), dimensions, contents, isAdjustable);
	}
}
