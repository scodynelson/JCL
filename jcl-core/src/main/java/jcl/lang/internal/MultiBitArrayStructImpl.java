package jcl.lang.internal;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jcl.lang.ArrayStruct;
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
 * The {@link MultiBitArrayStructImpl} is the object representation of a Lisp 'bit-array' type.
 */
public class MultiBitArrayStructImpl extends MultiArrayStructImpl implements BitArrayStruct {

	public MultiBitArrayStructImpl(final ArrayType arrayType, final List<Integer> dimensions,
	                               final List<LispStruct> contents, final boolean isAdjustable) {
		super(arrayType, dimensions, BitType.INSTANCE, contents, isAdjustable);
	}

	public MultiBitArrayStructImpl(final ArrayType arrayType, final List<Integer> dimensions,
	                               final ArrayStruct displacedTo, final Integer displacedIndexOffset,
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
		final List<LispStruct> initialContents = Stream.generate(() -> initialElement)
		                                               .limit(totalSize)
		                                               .collect(Collectors.toList());

		final boolean adjustableBoolean = isAdjustable.booleanValue();
		final ArrayType arrayType = getArrayType(adjustableBoolean);
		return new MultiBitArrayStructImpl(arrayType, dimensionInts, initialContents, adjustableBoolean);
	}

	public static BitArrayStruct valueOf(final List<IntegerStruct> dimensions, final SequenceStruct initialContents,
	                                     final BooleanStruct isAdjustable) {
		final List<Integer> dimensionInts = dimensions.stream()
		                                              .map(IntegerStruct::intValue)
		                                              .collect(Collectors.toList());
		final List<LispStruct> validContents = getValidContents(dimensionInts, initialContents);

		final boolean adjustableBoolean = isAdjustable.booleanValue();
		final ArrayType arrayType = getArrayType(adjustableBoolean);
		return new MultiBitArrayStructImpl(arrayType, dimensionInts, validContents, adjustableBoolean);
	}

	public static BitArrayStruct valueOf(final List<IntegerStruct> dimensions, final BitArrayStruct displacedTo,
	                                     final IntegerStruct displacedIndexOffset, final BooleanStruct isAdjustable) {
		final List<Integer> dimensionInts = dimensions.stream()
		                                              .map(IntegerStruct::intValue)
		                                              .collect(Collectors.toList());

		// TODO: Total size of A be no smaller than the sum of the total size of B plus the offset 'n' supplied by the offset

		return new MultiBitArrayStructImpl(ArrayType.INSTANCE, dimensionInts, displacedTo,
		                                   displacedIndexOffset.intValue(),
		                                   isAdjustable.booleanValue());
	}

	public static BitArrayStruct valueOf(final List<IntegerStruct> dimensions, final IntegerStruct initialElement) {
		final List<Integer> dimensionInts = dimensions.stream()
		                                              .map(IntegerStruct::intValue)
		                                              .collect(Collectors.toList());
		final int totalSize = dimensionInts.stream()
		                                   .mapToInt(Integer::intValue)
		                                   .sum();
		final List<LispStruct> initialContents = Stream.generate(() -> initialElement)
		                                               .limit(totalSize)
		                                               .collect(Collectors.toList());

		return new MultiBitArrayStructImpl(SimpleArrayType.INSTANCE, dimensionInts, initialContents, false);
	}

	public static BitArrayStruct valueOf(final List<IntegerStruct> dimensions, final SequenceStruct initialContents) {
		final List<Integer> dimensionInts = dimensions.stream()
		                                              .map(IntegerStruct::intValue)
		                                              .collect(Collectors.toList());
		final List<LispStruct> validContents = getValidContents(dimensionInts, initialContents);

		return new MultiBitArrayStructImpl(SimpleArrayType.INSTANCE, dimensionInts, validContents, false);
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
	private static List<LispStruct> getValidContents(final List<Integer> dimensions,
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

		final List<LispStruct> validContents = new ArrayList<>();

		final int dimension = dimensions.get(0);
		if (initialContents.length() == dimension) {
			final List<Integer> subDimension = dimensions.subList(1, numberOfDimensions);

			for (final LispStruct contentToCheck : initialContents) {
				if (!(contentToCheck instanceof SequenceStruct)) {
					throw new SimpleErrorException(
							initialContents + " doesn't match array dimensions of #<" + BitType.INSTANCE + ' ' + dimension + ">.");
				}

				final SequenceStruct subContents = (SequenceStruct) contentToCheck;
				final List<LispStruct> validSubContents = getValidContents(subDimension, subContents);
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
		if (displacedTo == null) {
			return new MultiBitArrayStructImpl(getArrayType(isAdjustable), dimensions, contents, isAdjustable);
		} else {
			return new MultiBitArrayStructImpl(getArrayType(isAdjustable), dimensions, displacedTo,
			                                   displacedIndexOffset, isAdjustable);
		}
	}
}
