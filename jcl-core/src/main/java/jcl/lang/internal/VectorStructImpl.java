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

	public static <T extends LispStruct> VectorStruct<T> valueOf(final Integer size, final LispType elementType,
	                                                             final T initialElement, final boolean isAdjustable,
	                                                             final Integer fillPointer) {
		final List<T> initialContents = Stream.generate(() -> initialElement)
		                                      .limit(size)
		                                      .collect(Collectors.toList());
		final VectorType vectorType = getVectorType(isAdjustable, fillPointer);
		return new VectorStructImpl<>(vectorType, size, elementType, initialContents, isAdjustable, fillPointer);
	}

	public static <T extends LispStruct> VectorStruct<T> valueOf(final Integer size, final LispType elementType,
	                                                             final List<T> initialContents,
	                                                             final boolean isAdjustable,
	                                                             final Integer fillPointer) {
		final VectorType vectorType = getVectorType(isAdjustable, fillPointer);
		return new VectorStructImpl<>(vectorType, size, elementType, initialContents, isAdjustable, fillPointer);
	}

	public static <T extends LispStruct> VectorStruct<T> valueOf(final Integer size, final LispType elementType,
	                                                             final T initialElement) {
		final List<T> initialContents = Stream.generate(() -> initialElement)
		                                      .limit(size)
		                                      .collect(Collectors.toList());
		return new VectorStructImpl<>(SimpleVectorType.INSTANCE, size, elementType, initialContents, false, null);
	}

	public static <T extends LispStruct> VectorStruct<T> valueOf(final Integer size, final LispType elementType,
	                                                             final List<T> initialContents) {
		return new VectorStructImpl<>(SimpleVectorType.INSTANCE, size, elementType, initialContents, false, null);
	}

	/*
		Old Builders
	 */

	public static <T extends LispStruct> VectorStruct<T> valueOf(final List<T> contents) {
		return new VectorStructImpl<>(SimpleVectorType.INSTANCE, contents.size(), TType.INSTANCE, contents, false,
		                              null);
	}

	/**
	 * Gets the vector type from the provided isAdjustable and fillPointer values.
	 *
	 * @param isAdjustable
	 * 		whether or not the vector is adjustable
	 * @param fillPointer
	 * 		the vector fillPointer
	 *
	 * @return the matching vector type for the provided isAdjustable and fillPointer values
	 */
	private static VectorType getVectorType(final boolean isAdjustable, final Integer fillPointer) {
		return (isAdjustable || (fillPointer != null)) ? VectorType.INSTANCE : SimpleVectorType.INSTANCE;
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
	                                     final TYPE initialElement) {
		return null;
	}

	@Override
	public ArrayStruct<TYPE> adjustArray(final List<IntegerStruct> dimensions, final LispType elementType,
	                                     final SequenceStruct initialContents) {
		return null;
	}

	@Override
	public ArrayStruct<TYPE> adjustArray(final List<IntegerStruct> dimensions, final LispType elementType,
	                                     final ArrayStruct<TYPE> displacedTo, final IntegerStruct displacedIndexOffset) {
		return null;
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
		if (!IntegerStruct.ZERO.eq(axisNumber)) {
			throw new ErrorException("Subscript " + axisNumber + " is out of bounds for " + this + '.');
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
		final int indexInt = index.intValue();
		if ((indexInt < 0) || (indexInt >= totalSize)) {
			throw new ErrorException("Index " + index + " is out of bounds for " + this + '.');
		}
		return contents.get(indexInt);
	}

	@Override
	public TYPE setfRowMajorAref(final TYPE newElement, final IntegerStruct index) {
		final int indexInt = index.intValue();
		if ((indexInt < 0) || (indexInt >= totalSize)) {
			throw new ErrorException("Index " + index + " is out of bounds for " + this + '.');
		}
		contents.set(indexInt, newElement);
		return newElement;
	}

	private int rowMajorIndexInternal(final IntegerStruct... subscripts) {
		final int numberOfSubscripts = subscripts.length;

		if (numberOfSubscripts != 1) {
			throw new ErrorException(
					"Wrong number of subscripts, " + numberOfSubscripts + ", for array of rank 1.");
		}

		final int subscript = subscripts[0].intValue();
		if ((subscript < 0) || (subscript >= totalSize)) {
			throw new ErrorException("Subscript " + subscript + " is out of bounds for " + this + '.');
		}
		return subscript;
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
