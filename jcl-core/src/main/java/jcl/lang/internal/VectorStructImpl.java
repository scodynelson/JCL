package jcl.lang.internal;

import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.function.Consumer;
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
import jcl.lang.VectorStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.internal.number.IntegerStructImpl;
import jcl.lang.statics.PrinterVariables;
import jcl.type.LispType;
import jcl.type.SimpleVectorType;
import jcl.type.TType;
import jcl.type.VectorType;

/**
 * The {@link VectorStructImpl} is the object representation of a Lisp 'vector' type.
 *
 * @param <TYPE>
 * 		the type of the vector contents
 */
public class VectorStructImpl<TYPE extends LispStruct> extends ArrayStructImpl<TYPE> implements VectorStruct<TYPE> {

	protected Integer totalSize;

	protected List<TYPE> contents;

	protected Integer fillPointer;

	/**
	 * Protected constructor.
	 *
	 * @param vectorType
	 * 		the vector type
	 * @param size
	 * 		the vector size
	 * @param contents
	 * 		the vector contents
	 * @param elementType
	 * 		the vector elementType
	 * @param isAdjustable
	 * 		whether or not the vector is adjustable
	 * @param fillPointer
	 * 		the vector fillPointer
	 */
	VectorStructImpl(final VectorType vectorType, final Integer size, final LispType elementType,
	                 final List<TYPE> contents, final boolean isAdjustable, final Integer fillPointer) {
		super(vectorType, elementType, isAdjustable);

		totalSize = size;
		this.contents = contents;
		this.fillPointer = fillPointer;
	}

	/**
	 * Protected constructor.
	 *
	 * @param vectorType
	 * 		the vector type
	 * @param size
	 * 		the vector size
	 * @param displacedTo
	 * 		the array structure that this array structure will be displaced to for content values
	 * @param displacedIndexOffset
	 * 		the offset of the index lookup for the content value into the displaced array structure
	 * @param elementType
	 * 		the vector elementType
	 * @param isAdjustable
	 * 		whether or not the vector is adjustable
	 * @param fillPointer
	 * 		the vector fillPointer
	 */
	VectorStructImpl(final VectorType vectorType, final Integer size, final LispType elementType,
	                 final ArrayStruct<TYPE> displacedTo, final Integer displacedIndexOffset,
	                 final boolean isAdjustable, final Integer fillPointer) {
		super(vectorType, elementType, displacedTo, displacedIndexOffset, isAdjustable);

		totalSize = size;
		this.fillPointer = fillPointer;
	}

	public static class Builder<T extends LispStruct> {

		private final IntegerStruct size;
		private LispType elementType = TType.INSTANCE;
		@SuppressWarnings("unchecked")
		private T initialElement = (T) NILStruct.INSTANCE;
		private SequenceStruct initialContents;
		private BooleanStruct adjustable = NILStruct.INSTANCE;
		private IntegerStruct fillPointer;
		private ArrayStruct<T> displacedTo;
		private IntegerStruct displacedIndexOffset = IntegerStruct.ZERO;

		public Builder(final IntegerStruct size) {
			this.size = size;
		}

		public Builder<T> elementType(final LispType elementType) {
			this.elementType = elementType;
			return this;
		}

		public Builder<T> initialElement(final T initialElement) {
			this.initialElement = initialElement;
			return this;
		}

		public Builder<T> initialContents(final SequenceStruct initialContents) {
			this.initialContents = initialContents;
			return this;
		}

		public Builder<T> adjustable(final BooleanStruct adjustable) {
			this.adjustable = adjustable;
			return this;
		}

		public Builder<T> fillPointer(final IntegerStruct fillPointer) {
			this.fillPointer = fillPointer;
			return this;
		}

		public Builder<T> displacedTo(final ArrayStruct<T> displacedTo) {
			this.displacedTo = displacedTo;
			return this;
		}

		public Builder<T> displacedIndexOffset(final IntegerStruct displacedIndexOffset) {
			this.displacedIndexOffset = displacedIndexOffset;
			return this;
		}

		public VectorStruct<T> build() {
			final int sizeInt = size.intValue();
			final LispType upgradedET = ArrayStruct.upgradedArrayElementType(elementType);
			final boolean adjustableBoolean = adjustable.booleanValue();
			final Integer fillPointerInt = (fillPointer == null) ? null : fillPointer.intValue();

			if (displacedTo != null) {
				final LispType displacedToType = displacedTo.getType();
				if (!displacedToType.equals(upgradedET) && !upgradedET.equals(displacedToType)) {
					throw new TypeErrorException(
							"Provided displaced to " + displacedTo + " is not an array with a subtype of the upgraded-array-element-type " + upgradedET + '.');
				}

				try {
					displacedTo.rowMajorAref(displacedIndexOffset);
				} catch (final ErrorException ignore) {
					throw new ErrorException("Requested size is too large to displace to " + displacedTo + '.');
				}

				return new VectorStructImpl<>(VectorType.INSTANCE,
				                              sizeInt,
				                              upgradedET,
				                              displacedTo,
				                              displacedIndexOffset.intValue(),
				                              adjustableBoolean,
				                              fillPointerInt);
			}

			final VectorType vectorType = (adjustableBoolean || (fillPointerInt != null))
			                              ? VectorType.INSTANCE
			                              : SimpleVectorType.INSTANCE;

			if (initialContents != null) {
				for (final LispStruct element : initialContents) {
					final LispType initialElementType = element.getType();
					if (!initialElementType.equals(upgradedET) && !upgradedET.equals(initialElementType)) {
						throw new TypeErrorException(
								"Provided element " + element + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
					}
				}

				final List<T> validContents = getValidContents(Collections.singletonList(sizeInt),
				                                               elementType,
				                                               initialContents);
				return new VectorStructImpl<>(vectorType,
				                              sizeInt,
				                              elementType,
				                              validContents,
				                              adjustableBoolean,
				                              fillPointerInt);
			} else {
				final LispType initialElementType = initialElement.getType();
				if (!initialElementType.equals(upgradedET) && !upgradedET.equals(initialElementType)) {
					throw new TypeErrorException(
							"Provided element " + initialElement + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
				}

				final List<T> contents = Stream.generate(() -> initialElement)
				                               .limit(sizeInt)
				                               .collect(Collectors.toList());
				return new VectorStructImpl<>(vectorType,
				                              sizeInt,
				                              elementType,
				                              contents,
				                              adjustableBoolean,
				                              fillPointerInt);
			}
		}
	}

	/*
		Old Builders
	 */

	public static <T extends LispStruct> VectorStruct<T> valueOf(final List<T> contents) {
		return new VectorStructImpl<>(SimpleVectorType.INSTANCE,
		                              contents.size(),
		                              TType.INSTANCE,
		                              contents,
		                              false,
		                              null);
	}

	@Override
	public IntegerStruct fillPointer() {
		return IntegerStructImpl.valueOf(fillPointer);
	}

	@Override
	public IntegerStruct setfFillPointer(final IntegerStruct fillPointer) {
		this.fillPointer = fillPointer.intValue();
		return fillPointer;
	}

	@Override
	public TYPE vectorPop() {
		if (fillPointer == null) {
			throw new TypeErrorException("Cannot pop from a VECTOR with no fill-pointer.");
		}
		if (fillPointer == 0) {
			throw new ErrorException("Nothing left to pop.");
		}

		fillPointer--;
		final TYPE element = contents.get(fillPointer);
		contents.set(fillPointer, null);
		return element;
	}

	@Override
	public LispStruct vectorPush(final TYPE element) {
		if (fillPointer == null) {
			throw new TypeErrorException("Cannot push into a VECTOR with no fill-pointer.");
		}
		if (fillPointer >= contents.size()) {
			return NILStruct.INSTANCE;
		}

		final Integer previousFillPointer = fillPointer++;
		contents.set(fillPointer, element);
		return IntegerStructImpl.valueOf(previousFillPointer);
	}

	@Override
	public IntegerStruct vectorPushExtend(final TYPE element, final IntegerStruct extensionAmount) {
		if (fillPointer == null) {
			throw new TypeErrorException("Cannot push into a VECTOR with no fill-pointer.");
		}
		if (!isAdjustable) {
			throw new TypeErrorException("VECTOR is not adjustable.");
		}
		if (fillPointer >= contents.size()) {
//			adjustArray(fillPointer + extensionAmount);
		}

		final Integer previousFillPointer = fillPointer++;
		contents.set(fillPointer, element);
		return IntegerStructImpl.valueOf(previousFillPointer);
	}

	/*
	ARRAY-STRUCT
	 */

	@Override
	public ArrayStruct<TYPE> adjustArray(final List<IntegerStruct> dimensions, final LispType elementType,
	                                     final TYPE initialElement, final IntegerStruct fillPointer) {

		if (!dimensions.isEmpty()) {
			throw new ErrorException("Array cannot be adjusted to a different array dimension rank.");
		}
		final LispType upgradedET = ArrayStruct.upgradedArrayElementType(elementType);

		if (!this.elementType.equals(upgradedET) || !upgradedET.equals(this.elementType)) {
			throw new TypeErrorException(
					"Provided upgraded-array-element-type " + upgradedET + " must be the same as initial upgraded-array-element-type " + this.elementType + '.');
		}

		final LispType initialElementType = initialElement.getType();
		if (!initialElementType.equals(upgradedET) && !upgradedET.equals(initialElementType)) {
			throw new TypeErrorException(
					"Provided element " + initialElement + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
		}

		final IntegerStruct size = dimensions.get(0);
		if (isAdjustable) {
			this.elementType = upgradedET;
			contents = Stream.generate(() -> initialElement)
			                 .limit(size.intValue())
			                 .collect(Collectors.toList());
			displacedTo = null;
			displacedIndexOffset = 0;
			return this;
		} else {
			return new Builder<TYPE>(size).elementType(upgradedET)
			                              .initialElement(initialElement)
			                              .build();
		}
	}

	@Override
	public ArrayStruct<TYPE> adjustArray(final List<IntegerStruct> dimensions, final LispType elementType,
	                                     final SequenceStruct initialContents, final IntegerStruct fillPointer) {

		if (!dimensions.isEmpty()) {
			throw new ErrorException("Array cannot be adjusted to a different array dimension rank.");
		}
		final LispType upgradedET = ArrayStruct.upgradedArrayElementType(elementType);

		if (!this.elementType.equals(upgradedET) || !upgradedET.equals(this.elementType)) {
			throw new TypeErrorException(
					"Provided upgraded-array-element-type " + upgradedET + " must be the same as initial upgraded-array-element-type " + this.elementType + '.');
		}

		for (final LispStruct initialElement : initialContents) {
			final LispType initialElementType = initialElement.getType();
			if (!initialElementType.equals(upgradedET) && !upgradedET.equals(initialElementType)) {
				throw new TypeErrorException(
						"Provided element " + initialElement + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
			}
		}

		final IntegerStruct size = dimensions.get(0);
		if (isAdjustable) {
			this.elementType = upgradedET;
			final List<Integer> dimensionInts = Collections.singletonList(size.intValue());
			contents = getValidContents(dimensionInts, elementType, initialContents);
			displacedTo = null;
			displacedIndexOffset = 0;
			return this;
		} else {
			return new Builder<TYPE>(size).elementType(upgradedET)
			                              .initialContents(initialContents)
			                              .build();
		}
	}

	@Override
	public ArrayStruct<TYPE> adjustArray(final List<IntegerStruct> dimensions, final LispType elementType,
	                                     final IntegerStruct fillPointer, final ArrayStruct<TYPE> displacedTo,
	                                     final IntegerStruct displacedIndexOffset) {

		if (!dimensions.isEmpty()) {
			throw new ErrorException("Array cannot be adjusted to a different array dimension rank.");
		}
		final LispType upgradedET = ArrayStruct.upgradedArrayElementType(elementType);

		if (!this.elementType.equals(upgradedET) || !upgradedET.equals(this.elementType)) {
			throw new TypeErrorException(
					"Provided upgraded-array-element-type " + upgradedET + " must be the same as initial upgraded-array-element-type " + this.elementType + '.');
		}

		final LispType initialElementType = displacedTo.arrayElementType();
		if (!initialElementType.equals(upgradedET) || !upgradedET.equals(initialElementType)) {
			throw new TypeErrorException(
					"Provided array for displacement " + displacedTo + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
		}

		try {
			displacedTo.rowMajorAref(displacedIndexOffset);
		} catch (final ErrorException ignore) {
			throw new ErrorException("Requested size is too large to displace to " + displacedTo + '.');
		}

		final IntegerStruct size = dimensions.get(0);
		if (isAdjustable) {
			this.elementType = elementType;
			contents = null;
			this.displacedTo = displacedTo;
			this.displacedIndexOffset = displacedIndexOffset.intValue();
			return this;
		} else {
			return new Builder<TYPE>(size).elementType(upgradedET)
			                              .displacedTo(displacedTo)
			                              .displacedIndexOffset(displacedIndexOffset)
			                              .build();
		}
	}

	@Override
	public TYPE aref(final IntegerStruct... subscripts) {
		final int rowMajorIndex = rowMajorIndexInternal(subscripts);
		if (displacedTo == null) {
			return contents.get(rowMajorIndex);
		}

		final IntegerStruct indexToGet = IntegerStructImpl.valueOf(displacedIndexOffset + rowMajorIndex);
		return displacedTo.rowMajorAref(indexToGet);
	}

	@Override
	public TYPE setfAref(final TYPE newElement, final IntegerStruct... subscripts) {
		final int rowMajorIndex = rowMajorIndexInternal(subscripts);
		if (displacedTo == null) {
			contents.set(rowMajorIndex, newElement);
		} else {
			final IntegerStruct indexToSet = IntegerStructImpl.valueOf(displacedIndexOffset + rowMajorIndex);
			displacedTo.setfRowMajorAref(newElement, indexToSet);
		}
		return newElement;
	}

	@Override
	public IntegerStruct arrayDimension(final IntegerStruct axisNumber) {
		if (!IntegerStruct.ZERO.eq(axisNumber)) {
			throw new ErrorException("Axis " + axisNumber + " is out of bounds for " + this + '.');
		}
		return IntegerStructImpl.valueOf(totalSize);
	}

	@Override
	public ListStruct arrayDimensions() {
		final IntegerStruct size = IntegerStructImpl.valueOf(totalSize);
		return LispStructFactory.toProperList(size);
	}

	@Override
	public BooleanStruct arrayHasFillPointerP() {
		return LispStructFactory.toBoolean(fillPointer != null);
	}

	@Override
	public BooleanStruct arrayInBoundsP(final IntegerStruct... subscripts) {
		try {
			rowMajorIndexInternal(subscripts);
			return TStruct.INSTANCE;
		} catch (final ErrorException ignore) {
			return NILStruct.INSTANCE;
		}
	}

	@Override
	public IntegerStruct arrayRank() {
		return IntegerStruct.ONE;
	}

	@Override
	public IntegerStruct arrayRowMajorIndex(final IntegerStruct... subscripts) {
		final int rowMajorIndex = rowMajorIndexInternal(subscripts);
		return IntegerStructImpl.valueOf(rowMajorIndex);
	}

	@Override
	public IntegerStruct arrayTotalSize() {
		return IntegerStructImpl.valueOf(totalSize);
	}

	@Override
	public TYPE rowMajorAref(final IntegerStruct index) {
		final int indexInt = validateSubscript(index);
		return contents.get(indexInt);
	}

	@Override
	public TYPE setfRowMajorAref(final TYPE newElement, final IntegerStruct index) {
		final int indexInt = validateSubscript(index);
		contents.set(indexInt, newElement);
		return newElement;
	}

	private int rowMajorIndexInternal(final IntegerStruct... subscripts) {
		final int numberOfSubscripts = subscripts.length;

		if (numberOfSubscripts != 1) {
			throw new ErrorException(
					"Wrong number of subscripts, " + numberOfSubscripts + ", for array of rank 1.");
		}

		final IntegerStruct subscript = subscripts[0];
		return validateSubscript(subscript);
	}

	private int validateSubscript(final IntegerStruct subscript) {
		final int subscriptInt = subscript.intValue();
		if ((subscriptInt < 0) || (subscriptInt >= totalSize)) {
			throw new ErrorException("Subscript " + subscript + " is out of bounds for " + this + '.');
		}
		return subscriptInt;
	}

// =================

	@Override
	public List<TYPE> getContents() {
		return contents;
	}

	@Override
	public List<Integer> getDimensions() {
		return Collections.singletonList(totalSize);
	}

// =================

	/*
	ITERABLE
	 */

	@Override
	public Iterator<LispStruct> iterator() {
		return new VectorIterator<>(contents.iterator());
	}

	@Override
	public Spliterator<LispStruct> spliterator() {
		return Spliterators.spliterator(contents.iterator(),
		                                contents.size(),
		                                Spliterator.ORDERED |
				                                Spliterator.SIZED |
				                                Spliterator.IMMUTABLE |
				                                Spliterator.SUBSIZED
		);
	}

	private static final class VectorIterator<TYPE extends LispStruct> implements Iterator<LispStruct> {

		private Iterator<TYPE> iterator;

		private VectorIterator(final Iterator<TYPE> iterator) {
			this.iterator = iterator;
		}

		@Override
		public boolean hasNext() {
			return iterator.hasNext();
		}

		@Override
		public LispStruct next() {
			return iterator.next();
		}

		@Override
		public void forEachRemaining(final Consumer<? super LispStruct> action) {
			iterator.forEachRemaining(action);
		}
	}

	@Override
	public String toString() {
		// TODO: Ignoring *PRINT-LEVEL* and *PRINT-LENGTH*

		final boolean printArray = PrinterVariables.PRINT_ARRAY.getVariableValue().booleanValue();
		final boolean printReadably = PrinterVariables.PRINT_READABLY.getVariableValue().booleanValue();

		final StringBuilder stringBuilder = new StringBuilder();

		if (printArray || printReadably) {
			stringBuilder.append("#(");

			final int amountToPrint = (fillPointer == null) ? contents.size() : fillPointer;

			for (int i = 0; i < amountToPrint; i++) {
				final TYPE lispStruct = contents.get(i);
				final String printedLispStruct = lispStruct.toString();

				stringBuilder.append(printedLispStruct);

				if (i < (amountToPrint - 1)) {
					stringBuilder.append(' ');
				}
			}

			stringBuilder.append(')');
		} else {
			final String typeClassName = getType().getClass().getSimpleName().toUpperCase();

			stringBuilder.append("#<");
			stringBuilder.append(typeClassName);
			stringBuilder.append(' ');

			stringBuilder.append(arrayTotalSize());

			stringBuilder.append(" type ");

			final String elementTypeClassName = arrayElementType().getClass().getName().toUpperCase();
			stringBuilder.append(elementTypeClassName);

			if (fillPointer != null) {
				stringBuilder.append(" fill-pointer ");
				stringBuilder.append(fillPointer);
			}

			if (isAdjustable) {
				stringBuilder.append(" adjustable");
			}

			stringBuilder.append('>');
		}

		return stringBuilder.toString();
	}
}
