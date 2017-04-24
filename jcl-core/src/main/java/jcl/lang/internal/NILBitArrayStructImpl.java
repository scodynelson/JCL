package jcl.lang.internal;

import jcl.lang.ArrayStruct;
import jcl.lang.BitArrayStruct;
import jcl.lang.BooleanStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.SequenceStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.type.ArrayType;
import jcl.type.BitType;
import jcl.type.LispType;
import jcl.type.SimpleArrayType;

/**
 * The {@link NILBitArrayStructImpl} is the object representation of a Lisp 'bit-array' type.
 */
public class NILBitArrayStructImpl extends NILArrayStructImpl implements BitArrayStruct {

	public NILBitArrayStructImpl(final ArrayType arrayType, final LispStruct content,
	                             final boolean isAdjustable) {
		super(arrayType, BitType.INSTANCE, content, isAdjustable);
	}

	public NILBitArrayStructImpl(final ArrayType arrayType, final ArrayStruct displacedTo,
	                             final Integer displacedIndexOffset, final boolean isAdjustable) {
		super(arrayType, BitType.INSTANCE, displacedTo, displacedIndexOffset, isAdjustable);
	}

	public static BitArrayStruct valueOf(final IntegerStruct initialElement, final BooleanStruct isAdjustable) {
		final boolean adjustableBoolean = isAdjustable.booleanValue();
		final ArrayType arrayType = getArrayType(adjustableBoolean);
		return new NILBitArrayStructImpl(arrayType, initialElement, adjustableBoolean);
	}

	public static BitArrayStruct valueOf(final SequenceStruct initialContents, final BooleanStruct isAdjustable) {
		final LispType upgradedET = BitType.INSTANCE;

		for (final LispStruct initialElement : initialContents) {
			final LispType initialElementType = initialElement.getType();
			if (!upgradedET.typeEquals(initialElementType)) {
				throw new TypeErrorException(
						"Provided element " + initialElement + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
			}
		}

		final boolean adjustableBoolean = isAdjustable.booleanValue();
		final ArrayType arrayType = getArrayType(adjustableBoolean);
		// TODO
		return new NILBitArrayStructImpl(arrayType, initialContents, adjustableBoolean);
	}

	public static BitArrayStruct valueOf(final BitArrayStruct displacedTo, final IntegerStruct displacedIndexOffset,
	                                     final BooleanStruct isAdjustable) {
		try {
			displacedTo.rowMajorAref(displacedIndexOffset);
		} catch (final ErrorException ignored) {
			throw new ErrorException("Requested size is too large to displace to " + displacedTo + '.');
		}

		return new NILBitArrayStructImpl(ArrayType.INSTANCE, displacedTo, displacedIndexOffset.intValue(),
		                                 isAdjustable.booleanValue());
	}

	public static BitArrayStruct valueOf(final IntegerStruct initialElement) {
		return new NILBitArrayStructImpl(SimpleArrayType.INSTANCE, initialElement, false);
	}

	public static BitArrayStruct valueOf(final SequenceStruct initialContents) {
		final LispType upgradedET = BitType.INSTANCE;

		for (final LispStruct initialElement : initialContents) {
			final LispType initialElementType = initialElement.getType();
			if (!upgradedET.typeEquals(initialElementType)) {
				throw new TypeErrorException(
						"Provided element " + initialElement + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
			}
		}

		// TODO
		return new NILBitArrayStructImpl(SimpleArrayType.INSTANCE, initialContents, false);
	}

	@Override
	public BitArrayStruct copyBitArray() {
		if (displacedTo == null) {
			return new NILBitArrayStructImpl(getArrayType(isAdjustable), content, isAdjustable);
		} else {
			return new NILBitArrayStructImpl(getArrayType(isAdjustable), displacedTo, displacedIndexOffset,
			                                 isAdjustable);
		}
	}
}
