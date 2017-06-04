package jcl.lang.internal;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.function.Consumer;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jcl.lang.ArrayStruct;
import jcl.lang.BitVectorStruct;
import jcl.lang.BooleanStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.NILStruct;
import jcl.lang.SequenceStruct;
import jcl.lang.ValuesStruct;
import jcl.lang.VectorStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.statics.PrinterVariables;
import jcl.type.BitType;
import jcl.type.BitVectorType;
import jcl.type.LispType;
import jcl.type.SimpleBitVectorType;

/**
 * The {@link BitVectorStructImpl} is the object representation of a Lisp 'bit-vector' type.
 */
public final class BitVectorStructImpl extends AbstractBitVectorStructImpl {

	private static final Pattern BIT_PATTERN = Pattern.compile("[0|1]+");

	private List<IntegerStruct> contents;

	protected Integer fillPointer;

	protected boolean isAdjustable;

	protected ArrayStruct displacedTo;

	protected Integer displacedIndexOffset;

	public BitVectorStructImpl(final BitVectorType bitVectorType, final Integer size,
	                           final List<IntegerStruct> contents, final boolean isAdjustable,
	                           final Integer fillPointer) {
		super(bitVectorType, size);
		this.contents = contents;
		this.isAdjustable = isAdjustable;
		this.fillPointer = fillPointer;
	}

	public static BitVectorStruct valueOf(final Integer size, final IntegerStruct initialElement,
	                                      final boolean isAdjustable, final Integer fillPointer) {
		final List<IntegerStruct> initialContents = Stream.generate(() -> initialElement)
		                                                  .limit(size)
		                                                  .collect(Collectors.toList());
		final BitVectorType bitVectorType = getBitVectorType(isAdjustable, fillPointer);
		return new BitVectorStructImpl(bitVectorType, size, initialContents, isAdjustable, fillPointer);
	}

	public static BitVectorStruct valueOf(final Integer size, final List<IntegerStruct> initialContents,
	                                      final boolean isAdjustable, final Integer fillPointer) {
		final BitVectorType bitVectorType = getBitVectorType(isAdjustable, fillPointer);
		return new BitVectorStructImpl(bitVectorType, size, initialContents, isAdjustable, fillPointer);
	}

	public static BitVectorStruct valueOf(final Integer size, final IntegerStruct initialElement) {
		final List<IntegerStruct> initialContents = Stream.generate(() -> initialElement)
		                                                  .limit(size)
		                                                  .collect(Collectors.toList());
		return new BitVectorStructImpl(SimpleBitVectorType.INSTANCE, size, initialContents, false, null);
	}

	public static BitVectorStruct valueOf(final Integer size, final List<IntegerStruct> initialContents) {
		return new BitVectorStructImpl(SimpleBitVectorType.INSTANCE, size, initialContents, false, null);
	}

	/*
		Old Builders
	 */

	public static BitVectorStruct valueOf(final String bitString) {
		return new BitVectorStructImpl(SimpleBitVectorType.INSTANCE, bitString.length(), getBitList(bitString), false,
		                               null);
	}

	public static BitVectorStruct valueOfCont(final List<IntegerStruct> contents) {
		return new BitVectorStructImpl(SimpleBitVectorType.INSTANCE, contents.size(), contents, false, null);
	}

	/**
	 * Gets the bit-vector type from the provided isAdjustable and fillPointer values.
	 *
	 * @param isAdjustable
	 * 		whether or not the bit-vector is adjustable
	 * @param fillPointer
	 * 		the bit-vector fillPointer
	 *
	 * @return the matching bit-vector type for the provided isAdjustable and fillPointer values
	 */
	private static BitVectorType getBitVectorType(final boolean isAdjustable, final Integer fillPointer) {
		return (isAdjustable || (fillPointer != null)) ? BitVectorType.INSTANCE : SimpleBitVectorType.INSTANCE;
	}

	/**
	 * Gets a list of {@link IntegerStruct}s from the provided {@link String} value.
	 *
	 * @param bitString
	 * 		the Java string to convert to a list of {@link IntegerStruct}s
	 *
	 * @return a list of {@link IntegerStruct}s from the provided {@link String} value
	 */
	private static List<IntegerStruct> getBitList(final String bitString) {
		if (!bitString.isEmpty() && !BIT_PATTERN.matcher(bitString).matches()) {
			throw new TypeErrorException(
					"Input contains characters not of type " + BitType.INSTANCE + ": " + bitString + '.');
		}

		final List<IntegerStruct> bitList = new ArrayList<>(bitString.length());
		for (final char character : bitString.toCharArray()) {
			if (character == '0') {
				bitList.add(IntegerStruct.ZERO);
			} else if (character == '1') {
				bitList.add(IntegerStruct.ONE);
			}
		}
		return bitList;
	}

	/*
	VECTOR-STRUCT
	 */

	@Override
	public IntegerStruct fillPointer() {
		if (fillPointer == null) {
			throw new TypeErrorException("VECTOR has no fill-pointer to retrieve.");
		}
		return IntegerStruct.toLispInteger(fillPointer);
	}

	@Override
	public IntegerStruct setfFillPointer(final IntegerStruct fillPointer) {
		final int intValue = fillPointer.toJavaInt();
		if ((intValue < 0) || (intValue > totalSize)) {
			throw new ErrorException(
					"Fill-pointer " + fillPointer + " value is out of bounds for VECTOR with size " + totalSize + '.');
		}

		this.fillPointer = intValue;
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

		final LispStruct element = contents.get(--fillPointer);
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

		final Integer formerFillPointer = fillPointer++;
		contents.set(formerFillPointer, (IntegerStruct) newElement);
		return IntegerStruct.toLispInteger(formerFillPointer);
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

		final Integer formerFillPointer = fillPointer++;
		contents.set(formerFillPointer, (IntegerStruct) newElement);
		return IntegerStruct.toLispInteger(formerFillPointer);
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

		if (!upgradedET.typeEquals(this.elementType)) {
			throw new TypeErrorException(
					"Provided upgraded-array-element-type " + upgradedET + " must be the same as initial upgraded-array-element-type " + this.elementType + '.');
		}

		final LispType initialElementType = initialElement.getType();
		if (!upgradedET.typeEquals(initialElementType)) {
			throw new TypeErrorException(
					"Provided element " + initialElement + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
		}

		final IntegerStruct size = dimensions.get(0);
		if (isAdjustable) {
			this.elementType = upgradedET;
			contents = Stream.generate(() -> (IntegerStruct) initialElement)
			                 .limit(size.toJavaInt())
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

		if (!upgradedET.typeEquals(this.elementType)) {
			throw new TypeErrorException(
					"Provided upgraded-array-element-type " + upgradedET + " must be the same as initial upgraded-array-element-type " + this.elementType + '.');
		}

		for (final LispStruct initialElement : initialContents) {
			final LispType initialElementType = initialElement.getType();
			if (!upgradedET.typeEquals(initialElementType)) {
				throw new TypeErrorException(
						"Provided element " + initialElement + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
			}
		}

		final IntegerStruct size = dimensions.get(0);
		if (isAdjustable) {
			this.elementType = upgradedET;
			final List<Integer> dimensionInts = Collections.singletonList(size.toJavaInt());
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

		if (!upgradedET.typeEquals(this.elementType)) {
			throw new TypeErrorException(
					"Provided upgraded-array-element-type " + upgradedET + " must be the same as initial upgraded-array-element-type " + this.elementType + '.');
		}

		final LispType initialElementType = displacedTo.arrayElementType();
		if (!upgradedET.typeEquals(initialElementType)) {
			throw new TypeErrorException(
					"Provided array for displacement " + displacedTo + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
		}

		try {
			displacedTo.rowMajorAref(displacedIndexOffset);
		} catch (final ErrorException ignored) {
			throw new ErrorException("Requested size is too large to displace to " + displacedTo + '.');
		}

		final IntegerStruct size = dimensions.get(0);
		if (isAdjustable) {
			this.elementType = elementType;
			contents = null;
			this.displacedTo = displacedTo;
			this.displacedIndexOffset = displacedIndexOffset.toJavaInt();
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
	public BooleanStruct adjustableArrayP() {
		return LispStructFactory.toBoolean(isAdjustable);
	}

	@Override
	public LispStruct aref(final IntegerStruct... subscripts) {
		final IntegerStruct subscript = rowMajorIndexInternal(subscripts);
		final int rowMajorIndex = validateSubscript(subscript);
		if (displacedTo == null) {
			return contents.get(rowMajorIndex);
		}

		final IntegerStruct indexToGet = IntegerStruct.toLispInteger(displacedIndexOffset + rowMajorIndex);
		return displacedTo.rowMajorAref(indexToGet);
	}

	@Override
	public LispStruct setfAref(final LispStruct newElement, final IntegerStruct... subscripts) { // TODO: type check
		final IntegerStruct subscript = rowMajorIndexInternal(subscripts);
		final int rowMajorIndex = validateSubscript(subscript);
		if (displacedTo == null) {
			contents.set(rowMajorIndex, (IntegerStruct) newElement);
		} else {
			final IntegerStruct indexToSet = IntegerStruct.toLispInteger(displacedIndexOffset + rowMajorIndex);
			displacedTo.setfRowMajorAref(newElement, indexToSet);
		}
		return newElement;
	}

	@Override
	public BooleanStruct arrayHasFillPointerP() {
		return LispStructFactory.toBoolean(fillPointer != null);
	}

	@Override
	public ValuesStruct arrayDisplacement() {
		return (displacedTo == null)
		       ? ValuesStruct.valueOf(NILStruct.INSTANCE, IntegerStruct.ZERO)
		       : ValuesStruct.valueOf(displacedTo, IntegerStruct.toLispInteger(displacedIndexOffset));
	}

	@Override
	public LispStruct rowMajorAref(final IntegerStruct index) {
		final int indexInt = validateSubscript(index);
		return contents.get(indexInt);
	}

	@Override
	public LispStruct setfRowMajorAref(final LispStruct newElement, final IntegerStruct index) { // TODO: type check
		final int indexInt = validateSubscript(index);
		contents.set(indexInt, (IntegerStruct) newElement);
		return newElement;
	}

// =================

	@Override
	public List<LispStruct> getContents() {
		return Collections.emptyList();
	}

// =================

	/*
	ITERABLE
	 */

	@Override
	public Iterator<LispStruct> iterator() {
		return new BitVectorIterator<>(contents.iterator());
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

	private static final class BitVectorIterator<TYPE extends LispStruct> implements Iterator<LispStruct> {

		private Iterator<TYPE> iterator;

		private BitVectorIterator(final Iterator<TYPE> iterator) {
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
		final boolean printArray = PrinterVariables.PRINT_ARRAY.getVariableValue().booleanValue();

		final StringBuilder stringBuilder = new StringBuilder();

		if (printArray) {
			stringBuilder.append("#*");

			final int amountToPrint = (fillPointer == null) ? contents.size() : fillPointer;

			for (int i = 0; i < amountToPrint; i++) {
				final IntegerStruct integerStruct = contents.get(i);
				final String printedIntegerStruct = integerStruct.toString();

				stringBuilder.append(printedIntegerStruct);
			}
		} else {
			final String typeClassName = getType().getClass().getSimpleName().toUpperCase();

			stringBuilder.append("#<");
			stringBuilder.append(typeClassName);
			stringBuilder.append(' ');

			stringBuilder.append(arrayTotalSize());

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

	@Override
	public BitVectorStruct copyBitArray() {
		return new BitVectorStructImpl(getBitVectorType(isAdjustable, fillPointer), contents.size(), contents,
		                               isAdjustable, fillPointer);
	}

	@Override
	public List<IntegerStruct> getBVContents() {
		return contents;
	}
}
