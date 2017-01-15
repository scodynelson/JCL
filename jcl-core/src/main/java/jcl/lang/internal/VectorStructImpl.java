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
 */
public class VectorStructImpl extends ArrayStructImpl implements VectorStruct {

	protected Integer totalSize;

	private List<LispStruct> contents;

	protected Integer fillPointer;

	protected VectorStructImpl(final VectorType vectorType, final Integer size, final LispType elementType,
	                           final boolean isAdjustable, final Integer fillPointer) {
		super(vectorType, elementType, isAdjustable);
		totalSize = size;
		this.fillPointer = fillPointer;
	}

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
	public VectorStructImpl(final VectorType vectorType, final Integer size, final LispType elementType,
	                        final List<LispStruct> contents, final boolean isAdjustable, final Integer fillPointer) {
		this(vectorType, size, elementType, isAdjustable, fillPointer);
		this.contents = contents;
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
	public VectorStructImpl(final VectorType vectorType, final Integer size, final LispType elementType,
	                        final ArrayStruct displacedTo, final Integer displacedIndexOffset,
	                        final boolean isAdjustable, final Integer fillPointer) {
		super(vectorType, elementType, displacedTo, displacedIndexOffset, isAdjustable);

		totalSize = size;
		this.fillPointer = fillPointer;
	}

	/*
		Old Builders
	 */

	public static VectorStruct valueOf(final List<LispStruct> contents) {
		return new VectorStructImpl(SimpleVectorType.INSTANCE,
		                            contents.size(),
		                            TType.INSTANCE,
		                            contents,
		                            false,
		                            null);
	}

	/*
	VECTOR-STRUCT
	 */

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
	public LispStruct vectorPop() {
		if (fillPointer == null) {
			throw new TypeErrorException("Cannot pop from a VECTOR with no fill-pointer.");
		}
		if (fillPointer == 0) {
			throw new ErrorException("Nothing left to pop.");
		}

		fillPointer--;
		final LispStruct element = contents.get(fillPointer);
		contents.set(fillPointer, null);
		return element;
	}

	@Override
	public LispStruct vectorPush(final LispStruct newElement) { // TODO: type check
		if (fillPointer == null) {
			throw new TypeErrorException("Cannot push into a VECTOR with no fill-pointer.");
		}
		if (fillPointer >= contents.size()) {
			return NILStruct.INSTANCE;
		}

		final Integer previousFillPointer = fillPointer++;
		contents.set(fillPointer, newElement);
		return IntegerStructImpl.valueOf(previousFillPointer);
	}

	@Override
	public IntegerStruct vectorPushExtend(final LispStruct newElement,
	                                      final IntegerStruct extension) { // TODO: type check
		if (fillPointer == null) {
			throw new TypeErrorException("Cannot push into a VECTOR with no fill-pointer.");
		}
		if (!isAdjustable) {
			throw new TypeErrorException("VECTOR is not adjustable.");
		}
		if (fillPointer >= contents.size()) {
//			adjustArray(fillPointer + extensionAmount); // TODO
		}

		final Integer previousFillPointer = fillPointer++;
		contents.set(fillPointer, newElement);
		return IntegerStructImpl.valueOf(previousFillPointer);
	}

	/*
	ARRAY-STRUCT
	 */

	@Override
	public ArrayStruct adjustArray(final List<IntegerStruct> dimensions, final LispType elementType,
	                               final LispStruct initialElement, final IntegerStruct fillPointer) {

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
			return VectorStruct.builder(size)
			                   .elementType(upgradedET)
			                   .initialElement(initialElement)
			                   .build();
		}
	}

	@Override
	public ArrayStruct adjustArray(final List<IntegerStruct> dimensions, final LispType elementType,
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
			contents = ArrayStruct.getValidContents(dimensionInts, elementType, initialContents);
			displacedTo = null;
			displacedIndexOffset = 0;
			return this;
		} else {
			return VectorStruct.builder(size)
			                   .elementType(upgradedET)
			                   .initialContents(initialContents)
			                   .build();
		}
	}

	@Override
	public ArrayStruct adjustArray(final List<IntegerStruct> dimensions, final LispType elementType,
	                               final IntegerStruct fillPointer, final ArrayStruct displacedTo,
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
			return VectorStruct.builder(size)
			                   .elementType(upgradedET)
			                   .displacedTo(displacedTo)
			                   .displacedIndexOffset(displacedIndexOffset)
			                   .build();
		}
	}

	@Override
	public LispStruct aref(final IntegerStruct... subscripts) {
		final int rowMajorIndex = rowMajorIndexInternal(subscripts);
		if (displacedTo == null) {
			return contents.get(rowMajorIndex);
		}

		final IntegerStruct indexToGet = IntegerStructImpl.valueOf(displacedIndexOffset + rowMajorIndex);
		return displacedTo.rowMajorAref(indexToGet);
	}

	@Override
	public LispStruct setfAref(final LispStruct newElement, final IntegerStruct... subscripts) { // TODO: type check
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
	public LispStruct rowMajorAref(final IntegerStruct index) {
		final int indexInt = validateSubscript(index);
		return contents.get(indexInt);
	}

	@Override
	public LispStruct setfRowMajorAref(final LispStruct newElement, final IntegerStruct index) { // TODO: type check
		final int indexInt = validateSubscript(index);
		contents.set(indexInt, newElement);
		return newElement;
	}

	protected int rowMajorIndexInternal(final IntegerStruct... subscripts) {
		final int numberOfSubscripts = subscripts.length;

		if (numberOfSubscripts != 1) {
			throw new ErrorException(
					"Wrong number of subscripts, " + numberOfSubscripts + ", for array of rank 1.");
		}

		final IntegerStruct subscript = subscripts[0];
		return validateSubscript(subscript);
	}

	protected int validateSubscript(final IntegerStruct subscript) {
		final int subscriptInt = subscript.intValue();
		if ((subscriptInt < 0) || (subscriptInt >= totalSize)) {
			throw new ErrorException("Subscript " + subscript + " is out of bounds for " + this + '.');
		}
		return subscriptInt;
	}

// =================

	@Override
	public List<LispStruct> getContents() {
		return contents;
	}

	@Override
	public List<Integer> getDimensions() {
		return Collections.singletonList(totalSize);
	}

// =================

	/*
	SEQUENCE-STRUCT
	 */

	@Override
	public IntegerStruct length() {
		if (fillPointer != null) {
			return IntegerStructImpl.valueOf(fillPointer);
		}
		return IntegerStructImpl.valueOf(totalSize);
	}

	@Override
	public LispStruct elt(final IntegerStruct index) {
		final int indexInt = validateIndexAgainstFillPointer(index);
		return contents.get(indexInt);
	}

	@Override
	public LispStruct setfElt(final LispStruct newElement, final IntegerStruct index) { // TODO: type check
		final int indexInt = validateIndexAgainstFillPointer(index);
		contents.set(indexInt, newElement);
		return newElement;
	}

	protected int validateIndexAgainstFillPointer(final IntegerStruct index) {
		if (fillPointer != null) {
			final int indexInt = index.intValue();
			if (indexInt > fillPointer) {
				throw new ErrorException(index + " is not a valid sequence index for " + this);
			}
		}
		return validateSubscript(index);
	}

	@Override
	public SequenceStruct reverse() {
		return this; // TODO
	}

	@Override
	public SequenceStruct nReverse() {
		return this; // TODO
	}

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

	/*
	OBJECT
	 */

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
				final LispStruct lispStruct = contents.get(i);
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
