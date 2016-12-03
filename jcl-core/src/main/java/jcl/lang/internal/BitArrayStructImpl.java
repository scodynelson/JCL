package jcl.lang.internal;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jcl.lang.BitArrayStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.SequenceStruct;
import jcl.lang.condition.exception.SimpleErrorException;
import jcl.lang.condition.exception.TypeErrorException;
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
	BitArrayStructImpl(final ArrayType arrayType, final List<Integer> dimensions, final boolean isAdjustable,
	                             final BitArrayStruct displacedTo, final Integer displacedIndexOffset) {
		super(arrayType, dimensions, BitType.INSTANCE, isAdjustable, displacedTo, displacedIndexOffset);
	}

	public static BitArrayStruct valueOf(final List<Integer> dimensions, final LispStruct initialContents,
	                                     final boolean isAdjustable) {

		if (!dimensions.isEmpty() && !(initialContents instanceof SequenceStruct)) {
			throw new TypeErrorException(
					"Expected :initial-contents value to be of type SEQUENCE. Got " + initialContents + '.');
		}

		final SequenceStruct initialContentsSeq = (SequenceStruct) initialContents;
		final List<IntegerStruct> validContents = getValidContents(dimensions, initialContentsSeq);

		final ArrayType arrayType = getArrayType(isAdjustable);
		return new BitArrayStructImpl(arrayType, dimensions, validContents, isAdjustable);
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

		return new BitArrayStructImpl(SimpleArrayType.INSTANCE, dimensions, initialContents, false);
	}

	public static BitArrayStruct valueOf(final List<Integer> dimensions, final LispStruct initialContents) {

		if (!dimensions.isEmpty() && !(initialContents instanceof SequenceStruct)) {
			throw new TypeErrorException(
					"Expected :initial-contents value to be of type SEQUENCE. Got " + initialContents + '.');
		}

		final SequenceStruct initialContentsSeq = (SequenceStruct) initialContents;
		final List<IntegerStruct> validContents = getValidContents(dimensions, initialContentsSeq);

		return new BitArrayStructImpl(SimpleArrayType.INSTANCE, dimensions, validContents, false);
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

		if (dimensions.size() == 1) {
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
			final List<Integer> subDimension = dimensions.subList(1, dimensions.size());

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
