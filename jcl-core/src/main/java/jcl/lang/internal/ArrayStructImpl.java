package jcl.lang.internal;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jcl.lang.ArrayStruct;
import jcl.lang.BooleanStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.SequenceStruct;
import jcl.lang.TStruct;
import jcl.lang.ValuesStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.SimpleErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.internal.number.IntegerStructImpl;
import jcl.lang.statics.PrinterVariables;
import jcl.type.ArrayType;
import jcl.type.LispType;
import jcl.type.SimpleArrayType;
import jcl.type.TType;
import org.apache.commons.math3.exception.OutOfRangeException;
import org.apache.commons.math3.util.MultidimensionalCounter;

/**
 * The {@link ArrayStructImpl} is the object representation of a Lisp 'array' type.
 *
 * @param <TYPE>
 * 		the type of the array contents
 */
public class ArrayStructImpl<TYPE extends LispStruct> extends BuiltInClassStruct implements ArrayStruct<TYPE> {

	protected List<TYPE> contents;

	protected List<Integer> dimensions;

	private MultidimensionalCounter multidimensionalCounter;

	private LispType elementType;

	boolean isAdjustable;

	private ArrayStruct<TYPE> displacedTo;

	private Integer displacedIndexOffset;

	/**
	 * Protected constructor.
	 *
	 * @param arrayType
	 * 		the array type
	 * @param dimensions
	 * 		the array dimensions
	 * @param contents
	 * 		the array contents
	 * @param elementType
	 * 		the array elementType
	 * @param isAdjustable
	 * 		whether or not the array is adjustable
	 */
	ArrayStructImpl(final ArrayType arrayType, final List<Integer> dimensions, final LispType elementType,
	                final List<TYPE> contents, final boolean isAdjustable) {
		super(arrayType, null, null);

		final int[] dimensionArray = dimensions.stream()
		                                       .mapToInt(Integer::intValue)
		                                       .toArray();
		multidimensionalCounter = new MultidimensionalCounter(dimensionArray);

		this.elementType = elementType;
		this.isAdjustable = isAdjustable;

		this.dimensions = dimensions;
		this.contents = contents;

		displacedTo = null;
		displacedIndexOffset = null;
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
	ArrayStructImpl(final ArrayType arrayType, final List<Integer> dimensions, final LispType elementType,
	                final ArrayStruct<TYPE> displacedTo, final Integer displacedIndexOffset,
	                final boolean isAdjustable) {
		super(arrayType, null, null);

		final int[] dimensionArray = dimensions.stream()
		                                       .mapToInt(Integer::intValue)
		                                       .toArray();
		multidimensionalCounter = new MultidimensionalCounter(dimensionArray);

		this.elementType = elementType;
		this.isAdjustable = isAdjustable;

		this.dimensions = dimensions;
		contents = null;

		this.displacedTo = displacedTo;
		this.displacedIndexOffset = displacedIndexOffset;
	}

	public static <T extends LispStruct> ArrayStruct<T> valueOf(final List<IntegerStruct> dimensions,
	                                                            final LispType elementType,
	                                                            final T initialElement,
	                                                            final BooleanStruct isAdjustable) {
		final List<Integer> dimensionInts = dimensions.stream()
		                                              .map(IntegerStruct::intValue)
		                                              .collect(Collectors.toList());

		final LispType initialElementType = initialElement.getType();
		if (!initialElementType.equals(elementType) && !elementType.equals(initialElementType)) {
			throw new TypeErrorException(
					"Provided element " + initialElement + " is not a subtype of the provided elementType " + elementType + '.');
		}

		final int totalSize = dimensionInts.stream()
		                                   .mapToInt(Integer::intValue)
		                                   .sum();
		final List<T> initialContents = Stream.generate(() -> initialElement)
		                                      .limit(totalSize)
		                                      .collect(Collectors.toList());

		final boolean adjustableBoolean = isAdjustable.booleanValue();
		final ArrayType arrayType = getArrayType(adjustableBoolean);
		return new ArrayStructImpl<>(arrayType, dimensionInts, elementType, initialContents, adjustableBoolean);
	}

	public static <T extends LispStruct> ArrayStruct<T> valueOf(final List<IntegerStruct> dimensions,
	                                                            final LispType elementType,
	                                                            final SequenceStruct initialContents,
	                                                            final BooleanStruct isAdjustable) {
		final List<Integer> dimensionInts = dimensions.stream()
		                                              .map(IntegerStruct::intValue)
		                                              .collect(Collectors.toList());
		final List<T> validContents = getValidContents(dimensionInts, elementType, initialContents);

		final boolean adjustableBoolean = isAdjustable.booleanValue();
		final ArrayType arrayType = getArrayType(adjustableBoolean);
		return new ArrayStructImpl<>(arrayType, dimensionInts, elementType, validContents, adjustableBoolean);
	}

	public static <T extends LispStruct> ArrayStruct<T> valueOf(final List<IntegerStruct> dimensions,
	                                                            final LispType elementType,
	                                                            final ArrayStruct<T> displacedTo,
	                                                            final IntegerStruct displacedIndexOffset,
	                                                            final BooleanStruct isAdjustable) {
		final List<Integer> dimensionInts = dimensions.stream()
		                                              .map(IntegerStruct::intValue)
		                                              .collect(Collectors.toList());

		// TODO: Total size of A be no smaller than the sum of the total size of B plus the offset 'n' supplied by the offset

		return new ArrayStructImpl<>(ArrayType.INSTANCE, dimensionInts, elementType, displacedTo,
		                             displacedIndexOffset.intValue(), isAdjustable.booleanValue());
	}

	public static <T extends LispStruct> ArrayStruct<T> valueOf(final List<IntegerStruct> dimensions,
	                                                            final LispType elementType,
	                                                            final T initialElement) {
		final List<Integer> dimensionInts = dimensions.stream()
		                                              .map(IntegerStruct::intValue)
		                                              .collect(Collectors.toList());

		final LispType initialElementType = initialElement.getType();
		if (!initialElementType.equals(elementType) && !elementType.equals(initialElementType)) {
			throw new TypeErrorException(
					"Provided element " + initialElement + " is not a subtype of the provided elementType " + elementType + '.');
		}

		final int totalSize = dimensionInts.stream()
		                                   .mapToInt(Integer::intValue)
		                                   .sum();
		final List<T> initialContents = Stream.generate(() -> initialElement)
		                                      .limit(totalSize)
		                                      .collect(Collectors.toList());

		return new ArrayStructImpl<>(SimpleArrayType.INSTANCE, dimensionInts, elementType, initialContents, false);
	}

	public static <T extends LispStruct> ArrayStruct<T> valueOf(final List<IntegerStruct> dimensions,
	                                                            final LispType elementType,
	                                                            final SequenceStruct initialContents) {
		final List<Integer> dimensionInts = dimensions.stream()
		                                              .map(IntegerStruct::intValue)
		                                              .collect(Collectors.toList());
		final List<T> validContents = getValidContents(dimensionInts, elementType, initialContents);

		return new ArrayStructImpl<>(SimpleArrayType.INSTANCE, dimensionInts, elementType, validContents, false);
	}

	/*
		Old Builders
	 */

	public static <T extends LispStruct> ArrayStruct<T> valueOf(final List<IntegerStruct> dimensions,
	                                                            final List<T> contents) {
		final List<Integer> dimensionInts = dimensions.stream()
		                                              .map(IntegerStruct::intValue)
		                                              .collect(Collectors.toList());

		return new ArrayStructImpl<>(SimpleArrayType.INSTANCE, dimensionInts, TType.INSTANCE, contents, false);
	}

	/**
	 * Gets the array type from the provided {@link #isAdjustable} value.
	 *
	 * @param isAdjustable
	 * 		whether or not the array is adjustable
	 *
	 * @return the matching array type for the provided {@link #isAdjustable} value
	 */
	static ArrayType getArrayType(final boolean isAdjustable) {
		return isAdjustable ? ArrayType.INSTANCE : SimpleArrayType.INSTANCE;
	}

	/**
	 * Determines if the provided {@code dimensionsToCheck} and {@code elementTypeToCheck} are valid for the provided
	 * {@code contentsToCheck}.
	 *
	 * @param dimensions
	 * 		the array dimensions to check
	 * @param elementType
	 * 		the array elementType to check
	 * @param initialContents
	 * 		the array contents to check
	 */
	private static <TYPE extends LispStruct> List<TYPE> getValidContents(final List<Integer> dimensions,
	                                                                     final LispType elementType,
	                                                                     final SequenceStruct initialContents) {
		final int numberOfDimensions = dimensions.size();
		if (numberOfDimensions == 0) {
			return Collections.emptyList();
		}

		if (numberOfDimensions == 1) {
			final int dimension = dimensions.get(0);
			if (initialContents.length() == dimension) {
				return getValidContents(elementType, initialContents);
			} else {
				throw new SimpleErrorException(
						initialContents + " doesn't match array dimensions of #<" + elementType + ' ' + dimension + ">.");
			}
		}

		final List<TYPE> validContents = new ArrayList<>();

		final int dimension = dimensions.get(0);
		if (initialContents.length() == dimension) {
			final List<Integer> subDimension = dimensions.subList(1, numberOfDimensions);

			for (final LispStruct contentToCheck : initialContents) {
				if (!(contentToCheck instanceof SequenceStruct)) {
					throw new SimpleErrorException(
							initialContents + " doesn't match array dimensions of #<" + elementType + ' ' + dimension + ">.");
				}

				final SequenceStruct subContents = (SequenceStruct) contentToCheck;
				final List<TYPE> validSubContents = getValidContents(subDimension, elementType, subContents);
				validContents.addAll(validSubContents);
			}
		} else {
			throw new SimpleErrorException(
					initialContents + " doesn't match array dimensions of #<" + elementType + ' ' + dimension + ">.");
		}

		return validContents;
	}

	@SuppressWarnings("unchecked")
	private static <TYPE extends LispStruct> List<TYPE> getValidContents(final LispType elementType,
	                                                                     final SequenceStruct initialContents) {
		final List<TYPE> validContents = new ArrayList<>();

		for (final LispStruct current : initialContents) {
			final LispType currentType = current.getType();
			if (!currentType.equals(elementType) && !elementType.equals(currentType)) {
				throw new TypeErrorException(
						"Provided element " + current + " is not a subtype of the provided elementType " + elementType + '.');
			} else {
				validContents.add((TYPE) current);
			}
		}
		return validContents;
	}

	@Override
	public ArrayStruct<TYPE> adjustArray(final List<IntegerStruct> dimensions, final LispType elementType,
	                                     final TYPE initialElement, final BooleanStruct isAdjustable) {
		return null;
	}

	@Override
	public ArrayStruct<TYPE> adjustArray(final List<IntegerStruct> dimensions, final LispType elementType,
	                                     final SequenceStruct initialContents, final BooleanStruct isAdjustable) {
		return null;
	}

	@Override
	public ArrayStruct<TYPE> adjustArray(final List<IntegerStruct> dimensions, final LispType elementType,
	                                     final ArrayStruct<TYPE> displacedTo, final IntegerStruct displacedIndexOffset,
	                                     final BooleanStruct isAdjustable) {
		return null;
	}

	@Override
	public ArrayStruct<TYPE> adjustArray(final List<IntegerStruct> newDimensions, final LispType elementType,
	                                     final TYPE initialElement) {
		return null;
	}

	@Override
	public ArrayStruct<TYPE> adjustArray(final List<IntegerStruct> newDimensions, final LispType elementType,
	                                     final SequenceStruct initialContents) {
		return null;
	}

	@Override
	public BooleanStruct adjustableArrayP() {
		return LispStructFactory.toBoolean(isAdjustable);
	}

	@Override
	public TYPE aref(final IntegerStruct... subscripts) {
		final int rowMajorIndex = rowMajorIndexInternal(subscripts);
		return contents.get(rowMajorIndex);
	}

	@Override
	public TYPE setfAref(final TYPE newElement, final IntegerStruct... subscripts) {
		final int rowMajorIndex = rowMajorIndexInternal(subscripts);
		contents.set(rowMajorIndex, newElement);
		return newElement;
	}

	@Override
	public IntegerStruct arrayDimension(final IntegerStruct axisNumber) {
		final int axisInt = axisNumber.intValue();
		final int[] sizes = multidimensionalCounter.getSizes();
		if ((axisInt < 0) || (axisInt >= sizes.length)) {
			throw new ErrorException("Subscript " + axisNumber + " is out of bounds for " + this + '.');
		}
		return IntegerStructImpl.valueOf(sizes[axisInt]);
	}

	@Override
	public ListStruct arrayDimensions() {
		final List<IntegerStruct> dimensionStructs
				= Arrays.stream(multidimensionalCounter.getSizes())
				        .mapToObj(IntegerStructImpl::valueOf)
				        .collect(Collectors.toList());
		return LispStructFactory.toProperList(dimensionStructs);
	}

	@Override
	public LispType arrayElementType() {
		return elementType;
	}

	@Override
	public BooleanStruct arrayHasFillPointerP() {
		return NILStruct.INSTANCE;
	}

	@Override
	public ValuesStruct arrayDisplacement() {
		return (displacedTo == null)
		       ? ValuesStruct.valueOf(NILStruct.INSTANCE, IntegerStruct.ZERO)
		       : ValuesStruct.valueOf(displacedTo, IntegerStructImpl.valueOf(displacedIndexOffset));
	}

	@Override
	public BooleanStruct arrayInBoundsP(final IntegerStruct... subscripts) {
		final int rank = multidimensionalCounter.getDimension();
		if (subscripts.length > rank) {
			throw new ErrorException(
					"Wrong number of subscripts, " + subscripts.length + ", for array of rank " + rank + '.');
		}

		final int[] intSubscripts
				= Arrays.stream(subscripts)
				        .mapToInt(IntegerStruct::intValue)
				        .toArray();

		try {
			multidimensionalCounter.getCount(intSubscripts);
			return TStruct.INSTANCE;
		} catch (final OutOfRangeException ignore) {
			return NILStruct.INSTANCE;
		}
	}

	@Override
	public IntegerStruct arrayRank() {
		final int rank = multidimensionalCounter.getDimension();
		return IntegerStructImpl.valueOf(rank);
	}

	@Override
	public IntegerStruct arrayRowMajorIndex(final IntegerStruct... subscripts) {
		final int rowMajorIndex = rowMajorIndexInternal(subscripts);
		return IntegerStructImpl.valueOf(rowMajorIndex);
	}

	@Override
	public IntegerStruct arrayTotalSize() {
		final int totalSize = multidimensionalCounter.getSize();
		return IntegerStructImpl.valueOf(totalSize);
	}

	@Override
	public TYPE rowMajorAref(final IntegerStruct index) {
		final int indexInt = index.intValue();
		final int totalSize = multidimensionalCounter.getSize();
		if (indexInt > totalSize) {
			throw new ErrorException("Index " + index + " is out of bounds for " + this + '.');
		}
		return contents.get(indexInt);
	}

	private int rowMajorIndexInternal(final IntegerStruct... subscripts) {
		final int rank = multidimensionalCounter.getDimension();
		if (subscripts.length > rank) {
			throw new ErrorException(
					"Wrong number of subscripts, " + subscripts.length + ", for array of rank " + rank + '.');
		}

		final int[] intSubscripts
				= Arrays.stream(subscripts)
				        .mapToInt(IntegerStruct::intValue)
				        .toArray();

		final int rowMajorIndex;
		try {
			rowMajorIndex = multidimensionalCounter.getCount(intSubscripts);
		} catch (final OutOfRangeException ex) {
			final Number argument = ex.getArgument();
			throw new ErrorException("Subscript " + argument + " is out of bounds for " + this + '.');
		}
		return rowMajorIndex;
	}

// =================

	@Override
	public List<TYPE> getContents() {
		return contents;
	}

	@Override
	public List<Integer> getDimensions() {
		return dimensions;
	}

// =================

	@Override
	public String toString() {
		// TODO: Ignoring *PRINT-LEVEL* and *PRINT-LENGTH*

		final boolean printArray = PrinterVariables.PRINT_ARRAY.getVariableValue().booleanValue();
		final boolean printReadably = PrinterVariables.PRINT_READABLY.getVariableValue().booleanValue();

		final StringBuilder stringBuilder = new StringBuilder();

		final int rank = multidimensionalCounter.getDimension();
		if (printArray || printReadably) {
			stringBuilder.append('#');

			stringBuilder.append(rank);
			stringBuilder.append('A');
			if (rank > 0) {
				stringBuilder.append('(');
			}

			final int contentsSize = contents.size();
			for (int i = 0; i < contentsSize; i++) {
				final TYPE lispStruct = contents.get(i);
				final String printedLispStruct = lispStruct.toString();

				stringBuilder.append(printedLispStruct);

				if (i < (contentsSize - 1)) {
					stringBuilder.append(' ');
				}
			}

			if (rank > 0) {
				stringBuilder.append(')');
			}
		} else {
			final String typeClassName = getType().getClass().getSimpleName().toUpperCase();

			stringBuilder.append("#<");
			stringBuilder.append(typeClassName);
			stringBuilder.append(' ');

			for (int i = 0; i < rank; i++) {
				stringBuilder.append(dimensions.get(i));

				if ((i + 1) != rank) {
					stringBuilder.append('x');
				}
			}

			stringBuilder.append(" type ");

			final String elementTypeClassName = elementType.getClass().getName().toUpperCase();
			stringBuilder.append(elementTypeClassName);

			if (isAdjustable) {
				stringBuilder.append(" adjustable");
			}

			stringBuilder.append('>');
		}

		return stringBuilder.toString();
	}
}
