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
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.internal.number.IntegerStructImpl;
import jcl.lang.statics.PrinterVariables;
import jcl.type.ArrayType;
import jcl.type.LispType;
import jcl.type.SimpleArrayType;
import jcl.type.TType;
import org.apache.commons.math3.exception.DimensionMismatchException;
import org.apache.commons.math3.exception.NotStrictlyPositiveException;
import org.apache.commons.math3.exception.OutOfRangeException;

public class MultiArrayStructImpl extends ArrayStructImpl {

	protected MultidimensionalCounter multidimensionalCounter;

	protected List<Integer> dimensions;

	protected List<LispStruct> contents;

	public MultiArrayStructImpl(final ArrayType arrayType, final List<Integer> dimensions, final LispType elementType,
	                            final List<LispStruct> contents, final boolean isAdjustable) {
		super(arrayType, elementType, isAdjustable);

		final int[] dimensionArray = dimensions.stream()
		                                       .mapToInt(Integer::intValue)
		                                       .toArray();
		multidimensionalCounter = new MultidimensionalCounter(dimensionArray);
		this.dimensions = dimensions;
		this.contents = contents;
	}

	public MultiArrayStructImpl(final ArrayType arrayType, final List<Integer> dimensions, final LispType elementType,
	                            final ArrayStruct displacedTo, final Integer displacedIndexOffset,
	                            final boolean isAdjustable) {
		super(arrayType, elementType, displacedTo, displacedIndexOffset, isAdjustable);

		final int[] dimensionArray = dimensions.stream()
		                                       .mapToInt(Integer::intValue)
		                                       .toArray();
		multidimensionalCounter = new MultidimensionalCounter(dimensionArray);
		this.dimensions = dimensions;
		contents = null;
	}

	public static ArrayStruct valueOf(final List<IntegerStruct> dimensions, final LispType elementType,
	                                  final LispStruct initialElement, final BooleanStruct isAdjustable) {
		final int rank = dimensions.size();
		if (rank == 0) {
			throw new IllegalStateException("Construct NILArray instead of MultiArray for a 0-rank array.");
		}
		if (rank == 1) {
			throw new IllegalStateException("Construct Vector instead of MultiArray for a 1-rank array.");
		}

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
		                                   .reduce(1, (x, y) -> x * y);
		final List<LispStruct> initialContents = Stream.generate(() -> initialElement)
		                                               .limit(totalSize)
		                                               .collect(Collectors.toList());

		final boolean adjustableBoolean = isAdjustable.booleanValue();
		final ArrayType arrayType = getArrayType(adjustableBoolean);
		return new MultiArrayStructImpl(arrayType, dimensionInts, elementType, initialContents, adjustableBoolean);
	}

	public static ArrayStruct valueOf(final List<IntegerStruct> dimensions, final LispType elementType,
	                                  final SequenceStruct initialContents, final BooleanStruct isAdjustable) {
		final int rank = dimensions.size();
		if (rank == 0) {
			throw new IllegalStateException("Construct NILArray instead of MultiArray for a 0-rank array.");
		}
		if (rank == 1) {
			throw new IllegalStateException("Construct Vector instead of MultiArray for a 1-rank array.");
		}

		final List<Integer> dimensionInts = dimensions.stream()
		                                              .map(IntegerStruct::intValue)
		                                              .collect(Collectors.toList());
		final List<LispStruct> validContents = ArrayStruct.getValidContents(dimensionInts, elementType,
		                                                                    initialContents);

		final boolean adjustableBoolean = isAdjustable.booleanValue();
		final ArrayType arrayType = getArrayType(adjustableBoolean);
		return new MultiArrayStructImpl(arrayType, dimensionInts, elementType, validContents, adjustableBoolean);
	}

	public static ArrayStruct valueOf(final List<IntegerStruct> dimensions, final LispType elementType,
	                                  final ArrayStruct displacedTo, final IntegerStruct displacedIndexOffset,
	                                  final BooleanStruct isAdjustable) {
		final int rank = dimensions.size();
		if (rank == 0) {
			throw new IllegalStateException("Construct NILArray instead of MultiArray for a 0-rank array.");
		}
		if (rank == 1) {
			throw new IllegalStateException("Construct Vector instead of MultiArray for a 1-rank array.");
		}


		// Error: Requested size is too large to displace to #<ARRAY 0-dimensional, simple> .


		final List<Integer> dimensionInts = dimensions.stream()
		                                              .map(IntegerStruct::intValue)
		                                              .collect(Collectors.toList());

		final int totalSize = dimensionInts.stream()
		                                   .mapToInt(Integer::intValue)
		                                   .reduce(1, (x, y) -> x * y);
		final int offsetInt = displacedIndexOffset.intValue();
		if (offsetInt > totalSize) {
			throw new ErrorException("Requested size is too large to displace to " + displacedTo + '.');
		}

		// TODO: Total size of A be no smaller than the sum of the total size of B plus the offset 'n' supplied by the offset

		return new MultiArrayStructImpl(ArrayType.INSTANCE, dimensionInts, elementType, displacedTo,
		                                offsetInt, isAdjustable.booleanValue());
	}

	public static ArrayStruct valueOf(final List<IntegerStruct> dimensions, final LispType elementType,
	                                  final LispStruct initialElement) {
		return valueOf(dimensions, elementType, initialElement, NILStruct.INSTANCE);
	}

	public static ArrayStruct valueOf(final List<IntegerStruct> dimensions, final LispType elementType,
	                                  final SequenceStruct initialContents) {
		return valueOf(dimensions, elementType, initialContents, NILStruct.INSTANCE);
	}

	/*
		Old Builders
	 */

	public static ArrayStruct valueOf(final List<IntegerStruct> dimensions, final List<LispStruct> contents) {
		final List<Integer> dimensionInts = dimensions.stream()
		                                              .map(IntegerStruct::intValue)
		                                              .collect(Collectors.toList());

		return new MultiArrayStructImpl(SimpleArrayType.INSTANCE, dimensionInts, TType.INSTANCE, contents, false);
	}

	/*
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
	*/

	private List<LispStruct> dataVectorFromInits(final Integer totalSize,
	                                             final List<LispStruct> initialContents, final boolean initialContentsP,
	                                             final LispStruct initialElement, final boolean initialElementP) {

		if (initialContentsP && initialElementP) {
			throw new ErrorException(
					"Cannot supply both :initial-contents and :initial-element to either make-array or adjust-array.");
		}

		List<LispStruct> data = new ArrayList<>();
		if (initialElementP) {
			data = Stream.generate(() -> initialElement)
			             .limit(totalSize)
			             .collect(Collectors.toList());
		} else if (initialContentsP) {
			data = initialContents;
		}

		return data;
	}

	private void zapArrayData(final List<LispStruct> oldData, final List<Integer> oldDims,
	                          final Integer offset,
	                          final List<LispStruct> newData, final List<Integer> newDims,
	                          final Integer newLength, final LispType elementType,
	                          final LispStruct initialElement, final boolean initialElementP) {

		Collections.reverse(oldDims);
		final List<Integer> innerNewDims = new ArrayList<>(newDims);
		Collections.reverse(innerNewDims);

		if (oldData == newData) {
			final List<LispStruct> temp = zapArrayDataTemp(newLength, elementType, initialElement, initialElementP);
			zapArrayDataAux(oldData, oldDims, offset, temp, newDims);

			for (int i = 0; i < newLength; i++) {
				newData.set(i, temp.get(i));
			}
		} else {
			zapArrayDataAux(oldData, oldDims, offset, newData, newDims);
		}
	}

	private List<LispStruct> zapArrayDataTemp(final Integer length, final LispType elementType,
	                                          final LispStruct initialElement, final boolean initialElementP) {
		return Stream.generate(() -> initialElement)
		             .limit(length)
		             .collect(Collectors.toList());
	}

	private void zapArrayDataAux(final List<LispStruct> oldData, final List<Integer> oldDims,
	                             final Integer offset,
	                             final List<LispStruct> newData, final List<Integer> newDims) {

		final List<Integer> limits = new ArrayList<>();
		for (int i = 0; i < oldDims.size(); i++) {
			final Integer x = oldDims.get(0);
			final Integer y = newDims.get(0);
			limits.add(Math.min(x, y) - 1);
		}

		List<Integer> indexList =
				Stream.generate(() -> 0)
				      .limit(oldDims.size())
				      .collect(Collectors.toList());

		while (!indexList.isEmpty()) {
			newData.set(rowMajorIndexFromDims(indexList, newDims),
			            oldData.get(rowMajorIndexFromDims(indexList, oldDims) + offset));

			indexList = bumpIndexList(indexList, limits);
		}
	}

	private List<Integer> bumpIndexList(final List<Integer> indexList, final List<Integer> limits) {

		List<Integer> subscripts = indexList;
		List<Integer> innerLimits = limits;

		while (!subscripts.isEmpty()) {

			if (subscripts.get(0) < innerLimits.get(0)) {
				subscripts.set(0, subscripts.get(0) + 1);
				return indexList;
			}
			subscripts.set(0, 0);

			subscripts = subscripts.subList(1, subscripts.size() - 1);
			innerLimits = innerLimits.subList(1, limits.size() - 1);
		}

		return Collections.emptyList();
	}

	private Integer rowMajorIndexFromDims(final List<Integer> revSubscripts, final List<Integer> revDimList) {

		List<Integer> innerRevSubscripts = revSubscripts;
		List<Integer> innerRevDimList = revDimList;
		Integer chunkSize = 1;
		Integer result = 0;

		while (!innerRevDimList.isEmpty()) {
			result += innerRevSubscripts.get(0) * chunkSize;
			chunkSize *= innerRevDimList.get(0);

			innerRevSubscripts = innerRevSubscripts.subList(1, innerRevSubscripts.size() - 1);
			innerRevDimList = innerRevDimList.subList(1, innerRevDimList.size() - 1);
		}
		return result;
	}

//	@Override
//	public ArrayStruct<TYPE> adjustArray(final List<IntegerStruct> dimensions, final LispType elementType,
//	                                     final TYPE initialElement, final BooleanStruct isAdjustable) {
//
//		if (this.dimensions.size() != dimensions.size()) {
//			throw new ErrorException("Array cannot be adjusted to a different array dimension rank.");
//		}
//
////		final int oldTotalSize;
////		if (dimensions.isEmpty()) {
////			oldTotalSize = 0;
////		} else if (multidimensionalCounter == null) {
////			oldTotalSize = this.dimensions.get(0);
////		} else {
////			oldTotalSize = multidimensionalCounter.getSize();
////		}
//		final int oldTotalSize = multidimensionalCounter.getSize();
//
//		final int[] dimensionArray = dimensions.stream()
//		                                       .mapToInt(IntegerStruct::intValue)
//		                                       .toArray();
//		final MultidimensionalCounter newMultidimensionalCounter = new MultidimensionalCounter(dimensionArray);
//		final int newTotalSize = newMultidimensionalCounter.getSize();
//
//		List<TYPE> newData;
//		if ((displacedTo != null) || (newTotalSize > oldTotalSize)) {
//			newData = dataVectorFromInits(newTotalSize, null, false, initialElement, true);
//		} else {
//			newData = contents;
//		}
//
//		final List<Integer> newDims = dimensions.stream().map(IntegerStruct::intValue).collect(Collectors.toList());
//		zapArrayData(contents, this.dimensions, displacedIndexOffset, newData, newDims, newTotalSize, elementType,
//		             initialElement, true);
//
//		this.contents = newData;
//		this.multidimensionalCounter = newMultidimensionalCounter;
//		this.isAdjustable = isAdjustable.booleanValue();
//		this.displacedTo = null;
//		this.displacedIndexOffset = 0;
//		this.dimensions = newDims;
//
//		return this;
//	}
//
//	@Override
//	public ArrayStruct<TYPE> adjustArray(final List<IntegerStruct> dimensions, final LispType elementType,
//	                                     final SequenceStruct initialContents, final BooleanStruct isAdjustable) {
//
//		if (this.dimensions.size() != dimensions.size()) {
//			throw new ErrorException("Array cannot be adjusted to a different array dimension rank.");
//		}
//
//		final List<Integer> dimensionInts = dimensions.stream()
//		                                              .map(IntegerStruct::intValue)
//		                                              .collect(Collectors.toList());
//		final List<TYPE> validContents = getValidContents(dimensionInts, elementType, initialContents);
//
//		final boolean adjustableBoolean = isAdjustable.booleanValue();
//		final ArrayType arrayType = getArrayType(adjustableBoolean);
////		return new ArrayStructImpl<>(arrayType, dimensionInts, elementType, validContents, adjustableBoolean);
//
//
//		final int[] dimensionArray = dimensions.stream()
//		                                       .mapToInt(IntegerStruct::intValue)
//		                                       .toArray();
//		multidimensionalCounter = new MultidimensionalCounter(dimensionArray);
//
//		this.elementType = elementType;
//		this.isAdjustable = isAdjustable.booleanValue();
//
//		this.dimensions = dimensions.stream().map(IntegerStruct::intValue).collect(Collectors.toList());
//		this.contents = new ArrayList<>(contents);
//
//		displacedTo = null;
//		displacedIndexOffset = 0;
//
//		return this;
//	}

	@Override
	public ArrayStruct adjustArray(final List<IntegerStruct> dimensions, final LispType elementType,
	                               final LispStruct initialElement, final IntegerStruct fillPointer) {

		if (this.dimensions.size() != dimensions.size()) {
			throw new ErrorException("Array cannot be adjusted to a different array dimension rank.");
		}

		final int oldTotalSize = multidimensionalCounter.getSize();

		final int[] dimensionArray = dimensions.stream()
		                                       .mapToInt(IntegerStruct::intValue)
		                                       .toArray();
		final MultidimensionalCounter newMultidimensionalCounter = new MultidimensionalCounter(dimensionArray);
		final int newTotalSize = newMultidimensionalCounter.getSize();

		List<LispStruct> newData;
		if ((displacedTo != null) || (newTotalSize > oldTotalSize)) {
			newData = dataVectorFromInits(newTotalSize, null, false, initialElement, true);
		} else {
			newData = contents;
		}

		final List<Integer> newDims = dimensions.stream().map(IntegerStruct::intValue).collect(Collectors.toList());
		zapArrayData(contents, this.dimensions, displacedIndexOffset, newData, newDims, newTotalSize, elementType,
		             initialElement, true);

		this.contents = newData;
		this.multidimensionalCounter = newMultidimensionalCounter;
		this.isAdjustable = false;
		this.displacedTo = null;
		this.displacedIndexOffset = 0;
		this.dimensions = newDims;

		return this;
	}

	@Override
	public ArrayStruct adjustArray(final List<IntegerStruct> dimensions, final LispType elementType,
	                               final SequenceStruct initialContents, final IntegerStruct fillPointer) {

		if (this.dimensions.size() != dimensions.size()) {
			throw new ErrorException("Array cannot be adjusted to a different array dimension rank.");
		}

		final List<Integer> dimensionInts = dimensions.stream()
		                                              .map(IntegerStruct::intValue)
		                                              .collect(Collectors.toList());
		final List<LispStruct> validContents = ArrayStruct.getValidContents(dimensionInts, elementType,
		                                                                    initialContents);

//		return new ArrayStructImpl<>(arrayType, dimensionInts, elementType, validContents, adjustableBoolean);


		final int[] dimensionArray = dimensions.stream()
		                                       .mapToInt(IntegerStruct::intValue)
		                                       .toArray();
		multidimensionalCounter = new MultidimensionalCounter(dimensionArray);

		this.elementType = elementType;
		this.isAdjustable = false;

		this.dimensions = dimensions.stream().map(IntegerStruct::intValue).collect(Collectors.toList());
		this.contents = new ArrayList<>(contents);

		displacedTo = null;
		displacedIndexOffset = 0;

		return this;
	}

	@Override
	public ArrayStruct adjustArray(final List<IntegerStruct> dimensions, final LispType elementType,
	                               final IntegerStruct fillPointer, final ArrayStruct displacedTo,
	                               final IntegerStruct displacedIndexOffset) {

		if (this.dimensions.size() != dimensions.size()) {
			throw new ErrorException("Array cannot be adjusted to a different array dimension rank.");
		}

		return null;
	}

	@Override
	public LispStruct aref(final IntegerStruct... subscripts) {
		final int rowMajorIndex = rowMajorIndexInternal(subscripts);
		return contents.get(rowMajorIndex);

//		validateSubscripts(subscripts);
//		if (displacedTo == null) {
//			return content;
//		}
//
//		final IntegerStruct index = displacedTo.arrayRowMajorIndex(subscripts);
//		// TODO: should the struct just stay persistent vs being unwrapped into an Integer???
//		final IntegerStruct displacedIndexOffsetStruct = IntegerStructImpl.valueOf(displacedIndexOffset);
//		final IntegerStruct indexWithOffset = (IntegerStruct) index.add(displacedIndexOffsetStruct);
//		return displacedTo.rowMajorAref(displacedIndexOffsetStruct);
	}

	@Override
	public LispStruct setfAref(final LispStruct newElement, final IntegerStruct... subscripts) { // TODO: type check
		final int rowMajorIndex = rowMajorIndexInternal(subscripts);
		contents.set(rowMajorIndex, newElement);
		return newElement;

//		validateSubscripts(subscripts);
//		if (displacedTo == null) {
//			content = newElement;
//		} else {
//			final IntegerStruct index = displacedTo.arrayRowMajorIndex(subscripts);
//			// TODO: should the struct just stay persistent vs being unwrapped into an Integer???
//			final IntegerStruct displacedIndexOffsetStruct = IntegerStructImpl.valueOf(displacedIndexOffset);
//			final IntegerStruct indexWithOffset = (IntegerStruct) index.add(displacedIndexOffsetStruct);
//			displacedTo.setfRowMajorAref(newElement, indexWithOffset);
//		}
//		return newElement;
	}

	@Override
	public IntegerStruct arrayDimension(final IntegerStruct axisNumber) {
		if (dimensions.isEmpty()) {
			throw new ErrorException("Cannot determine array dimension for array with rank 0.");
		}

		final int axisInt = axisNumber.intValue();
		final int[] sizes = multidimensionalCounter.getSizes();
		if ((axisInt < 0) || (axisInt >= sizes.length)) {
			throw new ErrorException("Subscript " + axisNumber + " is out of bounds for " + this + '.');
		}
		return IntegerStructImpl.valueOf(sizes[axisInt]);
	}

	@Override
	public ListStruct arrayDimensions() {
		final int[] sizes = multidimensionalCounter.getSizes();
		final List<IntegerStruct> dimensionStructs
				= Arrays.stream(sizes)
				        .mapToObj(IntegerStructImpl::valueOf)
				        .collect(Collectors.toList());
		return LispStructFactory.toProperList(dimensionStructs);
	}

	@Override
	public BooleanStruct arrayHasFillPointerP() {
		return NILStruct.INSTANCE;
	}

	@Override
	public BooleanStruct arrayInBoundsP(final IntegerStruct... subscripts) {
		try {
			rowMajorIndexInternal(subscripts);
			return TStruct.INSTANCE;
		} catch (final ErrorException ignored) {
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
	public LispStruct rowMajorAref(final IntegerStruct index) {
		final int indexInt = index.intValue();
		final int totalSize = multidimensionalCounter.getSize();
		if (indexInt > totalSize) {
			throw new ErrorException("Index " + index + " is out of bounds for " + this + '.');
		}
		return contents.get(indexInt);
	}

	@Override
	public LispStruct setfRowMajorAref(final LispStruct newElement, final IntegerStruct index) { // TODO: type check
		final int indexInt = index.intValue();
		final int totalSize = multidimensionalCounter.getSize();
		if (indexInt > totalSize) {
			throw new ErrorException("Index " + index + " is out of bounds for " + this + '.');
		}

		contents.set(indexInt, newElement);
		return newElement;
	}

	private int rowMajorIndexInternal(final IntegerStruct... subscripts) {
		final int numberOfSubscripts = subscripts.length;

		final int rank = multidimensionalCounter.getDimension();
		if (numberOfSubscripts != rank) {
			throw new ErrorException(
					"Wrong number of subscripts, " + numberOfSubscripts + ", for array of rank " + rank + '.');
		}

		final int[] intSubscripts
				= Arrays.stream(subscripts)
				        .mapToInt(IntegerStruct::intValue)
				        .toArray();

		final int rowMajorIndex;
		try {
			rowMajorIndex = multidimensionalCounter.getCount(intSubscripts);
		} catch (final DimensionMismatchException | OutOfRangeException ex) {
			final Number argument = ex.getArgument();
			throw new ErrorException("Subscript " + argument + " is out of bounds for " + this + '.');
		}
		return rowMajorIndex;
	}

// =================

	@Override
	public List<LispStruct> getContents() {
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
				final LispStruct lispStruct = contents.get(i);
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

	/*
		Adapted from commons-math3 implementation.
	 */

	/**
	 * Converter between unidimensional storage structure and multidimensional
	 * conceptual structure.
	 * This utility will convert from indices in a multidimensional structure
	 * to the corresponding index in a one-dimensional array. For example,
	 * assuming that the ranges (in 3 dimensions) of indices are 2, 4 and 3,
	 * the following correspondences, between 3-tuples indices and unidimensional
	 * indices, will hold:
	 * <ul>
	 * <li>(0, 0, 0) corresponds to 0</li>
	 * <li>(0, 0, 1) corresponds to 1</li>
	 * <li>(0, 0, 2) corresponds to 2</li>
	 * <li>(0, 1, 0) corresponds to 3</li>
	 * <li>...</li>
	 * <li>(1, 0, 0) corresponds to 12</li>
	 * <li>...</li>
	 * <li>(1, 3, 2) corresponds to 23</li>
	 * </ul>
	 */
	@SuppressWarnings("all")
	public static class MultidimensionalCounter {

		/**
		 * Number of dimensions.
		 */
		private final int dimension;
		/**
		 * Offset for each dimension.
		 */
		private final int[] uniCounterOffset;
		/**
		 * Counter sizes.
		 */
		private final int[] size;
		/**
		 * Total number of (one-dimensional) slots.
		 */
		private final int totalSize;
		/**
		 * Index of last dimension.
		 */
		private final int last;

		/**
		 * Create a counter.
		 *
		 * @param size
		 * 		Counter sizes (number of slots in each dimension).
		 *
		 * @throws NotStrictlyPositiveException
		 * 		if one of the sizes is
		 * 		negative or zero.
		 */
		public MultidimensionalCounter(int... size) throws NotStrictlyPositiveException {
			dimension = size.length;
			this.size = Arrays.copyOf(size, size.length);

			uniCounterOffset = new int[dimension];

			last = dimension - 1;
			if (dimension == 0) {
				totalSize = 1;
			} else {
				int tS = size[last];
				for (int i = 0; i < last; i++) {
					int count = 1;
					for (int j = i + 1; j < dimension; j++) {
						count *= size[j];
					}
					uniCounterOffset[i] = count;
					tS *= size[i];
				}
				uniCounterOffset[last] = 0;

				totalSize = tS;
			}
		}

		/**
		 * Get the number of dimensions of the multidimensional counter.
		 *
		 * @return the number of dimensions.
		 */
		public int getDimension() {
			return dimension;
		}

		/**
		 * Convert to multidimensional counter.
		 *
		 * @param index
		 * 		Index in unidimensional counter.
		 *
		 * @return the multidimensional counts.
		 *
		 * @throws OutOfRangeException
		 * 		if {@code index} is not between
		 * 		{@code 0} and the value returned by {@link #getSize()} (excluded).
		 */
		public int[] getCounts(int index) throws OutOfRangeException {
			if (index < 0 ||
					index >= totalSize) {
				throw new OutOfRangeException(index, 0, totalSize);
			}

			final int[] indices = new int[dimension];

			int count = 0;
			for (int i = 0; i < last; i++) {
				int idx = 0;
				final int offset = uniCounterOffset[i];
				while (count <= index) {
					count += offset;
					++idx;
				}
				--idx;
				count -= offset;
				indices[i] = idx;
			}

			indices[last] = index - count;

			return indices;
		}


		/**
		 * Convert to unidimensional counter.
		 *
		 * @param c
		 * 		Indices in multidimensional counter.
		 *
		 * @return the index within the unidimensionl counter.
		 *
		 * @throws DimensionMismatchException
		 * 		if the size of {@code c}
		 * 		does not match the size of the array given in the constructor.
		 * @throws OutOfRangeException
		 * 		if a value of {@code c} is not in
		 * 		the range of the corresponding dimension, as defined in the
		 * 		{@link org.apache.commons.math3.util.MultidimensionalCounter#MultidimensionalCounter(int...) constructor}.
		 */
		public int getCount(int... c)
				throws OutOfRangeException, DimensionMismatchException {
			if (c.length != dimension || dimension == 0) {
				throw new DimensionMismatchException(c.length, dimension);
			}
			int count = 0;
			for (int i = 0; i < dimension; i++) {
				final int index = c[i];
				if (index < 0 ||
						index >= size[i]) {
					throw new OutOfRangeException(index, 0, size[i] - 1);
				}
				count += uniCounterOffset[i] * c[i];
			}
			return count + c[last];
		}

		/**
		 * Get the total number of elements.
		 *
		 * @return the total size of the unidimensional counter.
		 */
		public int getSize() {
			return totalSize;
		}

		/**
		 * Get the number of multidimensional counter slots in each dimension.
		 *
		 * @return the sizes of the multidimensional counter in each dimension.
		 */
		public int[] getSizes() {
			return Arrays.copyOf(size, size.length);
		}

		/**
		 * {@inheritDoc}
		 */
		@Override
		public String toString() {
			final StringBuilder sb = new StringBuilder();
			for (int i = 0; i < dimension; i++) {
				sb.append("[").append(getCount(i)).append("]");
			}
			return sb.toString();
		}
	}
}
