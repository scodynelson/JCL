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
	private final MultidimensionalCounter multidimensionalCounter;

	protected LispType elementType;

	protected boolean isAdjustable;

	private final ArrayStruct<TYPE> displacedTo;
	private final Integer displacedIndexOffset;

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
	protected ArrayStructImpl(final ArrayType arrayType, final List<Integer> dimensions, final LispType elementType,
	                          final List<TYPE> contents, final boolean isAdjustable) {
		super(arrayType, null, null);

		// Check input data
		areContentsValidForDimensionsAndElementType(dimensions, elementType, contents);

		this.contents = new ArrayList<>(contents);
		this.dimensions = dimensions;

		final int[] dimensionArray = dimensions.stream()
		                                       .mapToInt(Integer::intValue)
		                                       .toArray();
		multidimensionalCounter = new MultidimensionalCounter(dimensionArray);

		this.elementType = elementType;
		this.isAdjustable = isAdjustable;

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
	private ArrayStructImpl(final ArrayType arrayType, final List<Integer> dimensions, final LispType elementType,
	                        final boolean isAdjustable, final ArrayStruct<TYPE> displacedTo,
	                        final Integer displacedIndexOffset) {
		super(arrayType, null, null);

		contents = null;
		this.dimensions = dimensions;

		final int[] dimensionArray = dimensions.stream()
		                                       .mapToInt(Integer::intValue)
		                                       .toArray();
		multidimensionalCounter = new MultidimensionalCounter(dimensionArray);

		this.elementType = elementType;
		this.isAdjustable = isAdjustable;

		this.displacedTo = displacedTo;
		this.displacedIndexOffset = displacedIndexOffset;
	}

	public static <T extends LispStruct> ArrayStruct<T> valueOf(final List<Integer> dimensions,
	                                                            final LispType elementType,
	                                                            final T initialElement, final boolean isAdjustable) {
		final int totalElements = dimensions.stream()
		                                    .mapToInt(Integer::intValue)
		                                    .sum();
		final List<T> initialContents = Stream.generate(() -> initialElement)
		                                      .limit(totalElements)
		                                      .collect(Collectors.toList());

		// Check input data
		// TODO: is this needed?? Optimize...
		areContentsValidForDimensionsAndElementType(dimensions, elementType, initialContents);

		final ArrayType arrayType = getArrayType(isAdjustable);
		return new ArrayStructImpl<>(arrayType, dimensions, elementType, initialContents, isAdjustable);
	}

	public static <T extends LispStruct> ArrayStruct<T> valueOf(final List<Integer> dimensions,
	                                                            final LispType elementType,
	                                                            final List<T> initialContents,
	                                                            final boolean isAdjustable) {

		// Check input data
		areContentsValidForDimensionsAndElementType(dimensions, elementType, initialContents);

		final ArrayType arrayType = getArrayType(isAdjustable);
		return new ArrayStructImpl<>(arrayType, dimensions, elementType, initialContents, isAdjustable);
	}

	public static <T extends LispStruct> ArrayStruct<T> valueOf(final List<Integer> dimensions,
	                                                            final LispType elementType,
	                                                            final boolean isAdjustable,
	                                                            final ArrayStruct<T> displacedTo,
	                                                            final Integer displacedIndexOffset) {
		return new ArrayStructImpl<>(ArrayType.INSTANCE, dimensions, elementType, isAdjustable, displacedTo,
		                             displacedIndexOffset);
	}

	public static <T extends LispStruct> ArrayStruct<T> valueOf(final List<Integer> dimensions,
	                                                            final LispType elementType,
	                                                            final T initialElement) {
		final int totalElements = dimensions.stream()
		                                    .mapToInt(Integer::intValue)
		                                    .sum();
		final List<T> initialContents = Stream.generate(() -> initialElement)
		                                      .limit(totalElements)
		                                      .collect(Collectors.toList());

		// Check input data
		// TODO: is this needed?? Optimize...
		areContentsValidForDimensionsAndElementType(dimensions, elementType, initialContents);

		return new ArrayStructImpl<>(SimpleArrayType.INSTANCE, dimensions, elementType, initialContents, false);
	}

	public static <T extends LispStruct> ArrayStruct<T> valueOf(final List<Integer> dimensions,
	                                                            final LispType elementType,
	                                                            final List<T> initialContents) {

		// Check input data
		areContentsValidForDimensionsAndElementType(dimensions, elementType, initialContents);

		return new ArrayStructImpl<>(SimpleArrayType.INSTANCE, dimensions, elementType, initialContents, false);
	}

	/*
		Old Builders
	 */

	public static <T extends LispStruct> ArrayStruct<T> valueOf(final List<Integer> dimensions,
	                                                            final List<T> contents) {
		return new ArrayStructImpl<>(SimpleArrayType.INSTANCE, dimensions, TType.INSTANCE, contents, false);
	}

	/**
	 * Gets the array type from the provided {@link #isAdjustable} value.
	 *
	 * @param isAdjustable
	 * 		whether or not the array is adjustable
	 *
	 * @return the matching array type for the provided {@link #isAdjustable} value
	 */
	private static ArrayType getArrayType(final boolean isAdjustable) {
		return isAdjustable ? ArrayType.INSTANCE : SimpleArrayType.INSTANCE;
	}

	/**
	 * Determines if the provided {@code dimensionsToCheck} and {@code elementTypeToCheck} are valid for the provided
	 * {@code contentsToCheck}.
	 *
	 * @param dimensionsToCheck
	 * 		the array dimensions to check
	 * @param elementTypeToCheck
	 * 		the array elementType to check
	 * @param contentsToCheck
	 * 		the array contents to check
	 */
	private static void areContentsValidForDimensionsAndElementType(final List<Integer> dimensionsToCheck,
	                                                                final LispType elementTypeToCheck,
	                                                                final List<? extends LispStruct> contentsToCheck) {

		if (dimensionsToCheck.isEmpty()) {
			return;
		}

		final Integer dimension = dimensionsToCheck.get(0);
		if (contentsToCheck.size() == dimension) {
			final List<Integer> subDimensionToCheck = dimensionsToCheck.subList(1, dimensionsToCheck.size());
			for (final LispStruct contentToCheck : contentsToCheck) {

				final List<LispStruct> subContentsToCheck;

				if (contentToCheck instanceof SequenceStruct) {
					final SequenceStruct sequenceToken = (SequenceStruct) contentToCheck;
					subContentsToCheck = sequenceToken.stream().collect(Collectors.toList());
				} else {
					subContentsToCheck = Collections.singletonList(contentToCheck);
				}
				areContentsValidForDimensionsAndElementType(subDimensionToCheck, elementTypeToCheck,
				                                            subContentsToCheck);
			}
		} else {
			throw new SimpleErrorException(
					contentsToCheck + " doesn't match array dimensions of #<" + elementTypeToCheck + ' ' + dimension + ">.");
		}

		for (final LispStruct current : contentsToCheck) {
			if (!current.getType().equals(elementTypeToCheck) && !elementTypeToCheck.equals(current.getType())) {
				throw new TypeErrorException(
						"Provided element " + current + " is not a subtype of the provided elementType " + elementTypeToCheck + '.');
			}
		}
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
