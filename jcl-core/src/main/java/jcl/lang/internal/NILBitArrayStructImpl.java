package jcl.lang.internal;

import jcl.lang.ArrayStruct;
import jcl.lang.BitArrayStruct;
import jcl.lang.BooleanStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LispStruct;
import jcl.lang.SequenceStruct;
import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.statics.CommonLispSymbols;

/**
 * The {@link NILBitArrayStructImpl} is the object representation of a Lisp 'bit-array' type.
 */
public class NILBitArrayStructImpl extends NILArrayStructImpl implements BitArrayStruct {

	public NILBitArrayStructImpl(final LispStruct content, final boolean isAdjustable) {
		super(CommonLispSymbols.BIT, content, isAdjustable);
	}

	public NILBitArrayStructImpl(final ArrayStruct displacedTo, final Integer displacedIndexOffset,
	                             final boolean isAdjustable) {
		super(CommonLispSymbols.BIT, displacedTo, displacedIndexOffset, isAdjustable);
	}

	public static BitArrayStruct valueOf(final IntegerStruct initialElement, final boolean isAdjustable) {
		return new NILBitArrayStructImpl(initialElement, isAdjustable);
	}

	public static BitArrayStruct valueOf(final SequenceStruct initialContents, final boolean isAdjustable) {
		final LispStruct upgradedET = CommonLispSymbols.BIT;

		for (final LispStruct initialElement : initialContents) {
			if (!initialElement.typep(upgradedET).toJavaPBoolean()) {
				throw new TypeErrorException(
						"Provided element " + initialElement + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
			}
		}

		// TODO
		return new NILBitArrayStructImpl(initialContents, isAdjustable);
	}

	public static BitArrayStruct valueOf(final BitArrayStruct displacedTo, final IntegerStruct displacedIndexOffset,
	                                     final BooleanStruct isAdjustable) {
		try {
			displacedTo.rowMajorAref(displacedIndexOffset);
		} catch (final ErrorException ignored) {
			throw new ErrorException("Requested size is too large to displace to " + displacedTo + '.');
		}

		return new NILBitArrayStructImpl(displacedTo, displacedIndexOffset.toJavaInt(), isAdjustable.toJavaPBoolean());
	}

	public static BitArrayStruct valueOf(final IntegerStruct initialElement) {
		return new NILBitArrayStructImpl(initialElement, false);
	}

	public static BitArrayStruct valueOf(final SequenceStruct initialContents) {
		final LispStruct upgradedET = CommonLispSymbols.BIT;

		for (final LispStruct initialElement : initialContents) {
			if (!initialElement.typep(upgradedET).toJavaPBoolean()) {
				throw new TypeErrorException(
						"Provided element " + initialElement + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
			}
		}

		// TODO
		return new NILBitArrayStructImpl(initialContents, false);
	}

	@Override
	public BitArrayStruct copyBitArray() {
		if (displacedTo == null) {
			return new NILBitArrayStructImpl(content, isAdjustable);
		} else {
			return new NILBitArrayStructImpl(displacedTo, displacedIndexOffset, isAdjustable);
		}
	}
}
