package jcl.lang.internal;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jcl.lang.ArrayStruct;
import jcl.lang.LispStruct;
import jcl.lang.SequenceStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.condition.exception.SimpleErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.statics.PrinterVariables;
import jcl.type.ArrayType;
import jcl.type.LispType;
import jcl.type.SimpleArrayType;
import jcl.type.TType;

/**
 * The {@link ArrayStructImpl} is the object representation of a Lisp 'array' type.
 *
 * @param <TYPE>
 * 		the type of the array contents
 */
public class ArrayStructImpl<TYPE extends LispStruct> extends BuiltInClassStruct implements ArrayStruct<TYPE> {

	protected List<TYPE> contents;

	protected List<Integer> dimensions;

	protected int totalSize;

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

		totalSize = 0;
		for (final Integer dimension : dimensions) {
			totalSize += dimension;
		}

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
	                        final boolean isAdjustable, final ArrayStruct<TYPE> displacedTo, final Integer displacedIndexOffset) {
		super(arrayType, null, null);

		contents = null;
		this.dimensions = dimensions;

		totalSize = 0;
		for (final Integer dimension : dimensions) {
			totalSize += dimension;
		}

		this.elementType = elementType;
		this.isAdjustable = isAdjustable;

		this.displacedTo = displacedTo;
		this.displacedIndexOffset = displacedIndexOffset;
	}

	public static <T extends LispStruct> ArrayStruct<T> valueOf(final List<Integer> dimensions, final LispType elementType,
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

	public static <T extends LispStruct> ArrayStruct<T> valueOf(final List<Integer> dimensions, final LispType elementType,
	                                                            final List<T> initialContents, final boolean isAdjustable) {

		// Check input data
		areContentsValidForDimensionsAndElementType(dimensions, elementType, initialContents);

		final ArrayType arrayType = getArrayType(isAdjustable);
		return new ArrayStructImpl<>(arrayType, dimensions, elementType, initialContents, isAdjustable);
	}

	public static <T extends LispStruct> ArrayStruct<T> valueOf(final List<Integer> dimensions, final LispType elementType,
	                                                            final boolean isAdjustable, final ArrayStruct<T> displacedTo,
	                                                            final Integer displacedIndexOffset) {
		return new ArrayStructImpl<>(ArrayType.INSTANCE, dimensions, elementType, isAdjustable, displacedTo, displacedIndexOffset);
	}

	public static <T extends LispStruct> ArrayStruct<T> valueOf(final List<Integer> dimensions, final LispType elementType,
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

	public static <T extends LispStruct> ArrayStruct<T> valueOf(final List<Integer> dimensions, final LispType elementType,
	                                                            final List<T> initialContents) {

		// Check input data
		areContentsValidForDimensionsAndElementType(dimensions, elementType, initialContents);

		return new ArrayStructImpl<>(SimpleArrayType.INSTANCE, dimensions, elementType, initialContents, false);
	}

	/*
		Old Builders
	 */

	public static <T extends LispStruct> ArrayStruct<T> valueOf(final List<Integer> dimensions, final List<T> contents) {
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
				areContentsValidForDimensionsAndElementType(subDimensionToCheck, elementTypeToCheck, subContentsToCheck);
			}
		} else {
			throw new SimpleErrorException(contentsToCheck + " doesn't match array dimensions of #<" + elementTypeToCheck + ' ' + dimension + ">.");
		}

		for (final LispStruct current : contentsToCheck) {
			if (!current.getType().equals(elementTypeToCheck) && !elementTypeToCheck.equals(current.getType())) {
				throw new TypeErrorException("Provided element " + current + " is not a subtype of the provided elementType " + elementTypeToCheck + '.');
			}
		}
	}

	@Override
	public List<TYPE> getContents() {
		return contents;
	}

	@Override
	public List<Integer> getDimensions() {
		return dimensions;
	}

	@Override
	public int getTotalSize() {
		return totalSize;
	}

	@Override
	public int getRank() {
		return dimensions.size();
	}

	@Override
	public LispType getElementType() {
		return elementType;
	}

	@Override
	public boolean isAdjustable() {
		return isAdjustable;
	}

	public TYPE getElementAt(final int... indicies) {
		return null;
	}

	@Override
	public TYPE getElementAt(final int index) {
		return contents.get(index);
	}

	@Override
	public void setElementAt(final int index, final TYPE newValue) {
		for (int i = contents.size(); i <= index; i++) {
			contents.add(null);
		}
		contents.set(index, newValue);
	}

	@Override
	public ArrayStruct<TYPE> getDisplacedTo() {
		return displacedTo;
	}

	@Override
	public Integer getDisplacedIndexOffset() {
		return displacedIndexOffset;
	}

	@Override
	public String toString() {
		// TODO: Ignoring *PRINT-LEVEL* and *PRINT-LENGTH*

		final boolean printArray = PrinterVariables.PRINT_ARRAY.getVariableValue().booleanValue();
		final boolean printReadably = PrinterVariables.PRINT_READABLY.getVariableValue().booleanValue();

		final StringBuilder stringBuilder = new StringBuilder();

		final int rank = dimensions.size();
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
