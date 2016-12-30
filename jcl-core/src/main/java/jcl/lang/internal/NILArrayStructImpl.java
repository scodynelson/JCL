package jcl.lang.internal;

import java.util.Collections;
import java.util.List;

import jcl.lang.ArrayStruct;
import jcl.lang.BooleanStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.NILStruct;
import jcl.lang.SequenceStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.factory.LispStructFactory;
import jcl.lang.internal.number.IntegerStructImpl;
import jcl.lang.statics.PrinterVariables;
import jcl.type.ArrayType;
import jcl.type.LispType;

public class NILArrayStructImpl<TYPE extends LispStruct> extends ArrayStructImpl<TYPE> {

	protected TYPE content;

	protected NILArrayStructImpl(final ArrayType arrayType, final LispType elementType,
	                             final TYPE content, final boolean isAdjustable) {
		super(arrayType, elementType, isAdjustable);
		this.content = content;
	}

	protected NILArrayStructImpl(final ArrayType arrayType, final LispType elementType,
	                             final ArrayStruct<TYPE> displacedTo, final Integer displacedIndexOffset,
	                             final boolean isAdjustable) {
		super(arrayType, elementType, displacedTo, displacedIndexOffset, isAdjustable);
	}

	public static <T extends LispStruct> ArrayStruct<T> valueOf(final LispType elementType, final T initialElement,
	                                                            final BooleanStruct isAdjustable) {
		final LispType upgradedET = ArrayStruct.upgradedArrayElementType(elementType);

		final LispType initialElementType = initialElement.getType();
		if (!initialElementType.equals(upgradedET) && !upgradedET.equals(initialElementType)) {
			throw new TypeErrorException(
					"Provided element " + initialElement + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
		}

		final boolean adjustableBoolean = isAdjustable.booleanValue();
		final ArrayType arrayType = getArrayType(adjustableBoolean);
		return new NILArrayStructImpl<>(arrayType, upgradedET, initialElement, adjustableBoolean);
	}

	@SuppressWarnings("unchecked")
	public static <T extends LispStruct> ArrayStruct<T> valueOf(final LispType elementType,
	                                                            final SequenceStruct initialContents,
	                                                            final BooleanStruct isAdjustable) {
		final LispType upgradedET = ArrayStruct.upgradedArrayElementType(elementType);

		for (final LispStruct initialElement : initialContents) {
			final LispType initialElementType = initialElement.getType();
			if (!initialElementType.equals(upgradedET) && !upgradedET.equals(initialElementType)) {
				throw new TypeErrorException(
						"Provided element " + initialElement + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
			}
		}

		final boolean adjustableBoolean = isAdjustable.booleanValue();
		final ArrayType arrayType = getArrayType(adjustableBoolean);
		return new NILArrayStructImpl<>(arrayType, upgradedET, (T) initialContents, adjustableBoolean);
	}

	public static <T extends LispStruct> ArrayStruct<T> valueOf(final LispType elementType,
	                                                            final ArrayStruct<T> displacedTo,
	                                                            final IntegerStruct displacedIndexOffset,
	                                                            final BooleanStruct isAdjustable) {

		final LispType displacedToType = displacedTo.getType();
		final LispType upgradedET = ArrayStruct.upgradedArrayElementType(elementType);
		if (!displacedToType.equals(upgradedET) && !upgradedET.equals(displacedToType)) {
			throw new TypeErrorException(
					"Provided displaced to " + displacedTo + " is not an array with a subtype of the upgraded-array-element-type " + upgradedET + '.');
		}

		try {
			displacedTo.rowMajorAref(displacedIndexOffset);
		} catch (final ErrorException ignore) {
			throw new ErrorException("Requested size is too large to displace to " + displacedTo + '.');
		}

		return new NILArrayStructImpl<>(ArrayType.INSTANCE, upgradedET, displacedTo, displacedIndexOffset.intValue(),
		                                isAdjustable.booleanValue());
	}

	public static <T extends LispStruct> ArrayStruct<T> valueOf(final LispType elementType, final T initialElement) {
		return valueOf(elementType, initialElement, NILStruct.INSTANCE);
	}

	@SuppressWarnings("unchecked")
	public static <T extends LispStruct> ArrayStruct<T> valueOf(final LispType elementType,
	                                                            final SequenceStruct initialContents) {
		return valueOf(elementType, initialContents, NILStruct.INSTANCE);
	}

	@Override
	public ArrayStruct<TYPE> adjustArray(final List<IntegerStruct> dimensions, final LispType elementType,
	                                     final TYPE initialElement) {

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

		if (isAdjustable) {
			this.elementType = upgradedET;
			content = initialElement;
			displacedTo = null;
			displacedIndexOffset = 0;
			return this;
		} else {
			return valueOf(upgradedET, initialElement);
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public ArrayStruct<TYPE> adjustArray(final List<IntegerStruct> dimensions, final LispType elementType,
	                                     final SequenceStruct initialContents) {

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

		if (isAdjustable) {
			this.elementType = upgradedET;
			content = (TYPE) initialContents;
			displacedTo = null;
			displacedIndexOffset = 0;
			return this;
		} else {
			return valueOf(upgradedET, initialContents);
		}
	}

	@Override
	public ArrayStruct<TYPE> adjustArray(final List<IntegerStruct> dimensions, final LispType elementType,
	                                     final ArrayStruct<TYPE> displacedTo,
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

		if (isAdjustable) {
			this.elementType = elementType;
			content = null;
			this.displacedTo = displacedTo;
			this.displacedIndexOffset = displacedIndexOffset.intValue();
			return this;
		} else {
			return valueOf(upgradedET, displacedTo, displacedIndexOffset, NILStruct.INSTANCE);
		}
	}

	@Override
	public TYPE aref(final IntegerStruct... subscripts) {
		validateSubscripts(subscripts);
		if (displacedTo == null) {
			return content;
		}

		// TODO: should the struct just stay persistent vs being unwrapped into an Integer???
		final IntegerStruct displacedIndexOffsetStruct = IntegerStructImpl.valueOf(displacedIndexOffset);
		return displacedTo.rowMajorAref(displacedIndexOffsetStruct);
	}

	@Override
	public TYPE setfAref(final TYPE newElement, final IntegerStruct... subscripts) {
		validateSubscripts(subscripts);
		if (displacedTo == null) {
			content = newElement;
		} else {
			// TODO: should the struct just stay persistent vs being unwrapped into an Integer???
			final IntegerStruct displacedIndexOffsetStruct = IntegerStructImpl.valueOf(displacedIndexOffset);
			displacedTo.setfRowMajorAref(newElement, displacedIndexOffsetStruct);
		}
		return newElement;
	}

	@Override
	public IntegerStruct arrayDimension(final IntegerStruct axisNumber) {
		throw new ErrorException("Cannot determine array dimension for array with rank 0.");
	}

	@Override
	public ListStruct arrayDimensions() {
		return NILStruct.INSTANCE;
	}

	@Override
	public BooleanStruct arrayHasFillPointerP() {
		return NILStruct.INSTANCE;
	}

	@Override
	public BooleanStruct arrayInBoundsP(final IntegerStruct... subscripts) {
		return LispStructFactory.toBoolean(subscripts.length == 0);
	}

	@Override
	public IntegerStruct arrayRank() {
		return IntegerStruct.ZERO;
	}

	@Override
	public IntegerStruct arrayRowMajorIndex(final IntegerStruct... subscripts) {
		validateSubscripts(subscripts);
		return IntegerStruct.ZERO;
	}

	@Override
	public IntegerStruct arrayTotalSize() {
		return IntegerStruct.ONE;
	}

	@Override
	public TYPE rowMajorAref(final IntegerStruct index) {
		if (!IntegerStruct.ZERO.eql(index)) {
			throw new ErrorException("Index " + index + " is out of bounds for " + this + '.');
		}
		return content;
	}

	@Override
	public TYPE setfRowMajorAref(final TYPE newElement, final IntegerStruct index) {
		if (!IntegerStruct.ZERO.eql(index)) {
			throw new ErrorException("Index " + index + " is out of bounds for " + this + '.');
		}
		content = newElement;
		return newElement;
	}

	private static void validateSubscripts(final IntegerStruct... subscripts) {
		final int numberOfSubscripts = subscripts.length;

		if (numberOfSubscripts != 0) {
			throw new ErrorException(
					"Wrong number of subscripts, " + numberOfSubscripts + ", for array of rank 0.");
		}
	}

// =================

	@Override
	public List<TYPE> getContents() {
		return Collections.singletonList(content);
	}

	@Override
	public List<Integer> getDimensions() {
		return Collections.emptyList();
	}

// =================

	@Override
	public String toString() {
		// TODO: Ignoring *PRINT-LEVEL* and *PRINT-LENGTH*

		final boolean printArray = PrinterVariables.PRINT_ARRAY.getVariableValue().booleanValue();
		final boolean printReadably = PrinterVariables.PRINT_READABLY.getVariableValue().booleanValue();

		final StringBuilder stringBuilder = new StringBuilder();

		if (printArray || printReadably) {
			stringBuilder.append("#0A");
			stringBuilder.append(content);

		} else {
			final String typeClassName = getType().getClass().getSimpleName().toUpperCase();

			stringBuilder.append("#<");
			stringBuilder.append(typeClassName);
			stringBuilder.append(" NIL type ");

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
