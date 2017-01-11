package jcl.lang;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.internal.BitVectorStructImpl;
import jcl.type.BitVectorType;
import jcl.type.LispType;
import jcl.type.SimpleBitVectorType;

/**
 * The {@link BitVectorStruct} is the object representation of a Lisp 'bit-vector' type.
 */
public interface BitVectorStruct extends VectorStruct<IntegerStruct>, BitArrayStruct {

	static BitVectorStruct.Builder builder(final IntegerStruct size) {
		return new BitVectorStruct.Builder(size);
	}

	class Builder extends VectorStruct.Builder<IntegerStruct> {

		public Builder(final IntegerStruct size) {
			super(size);
		}

		@Override
		public BitVectorStruct.Builder elementType(final LispType elementType) {
			this.elementType = elementType;
			return this;
		}

		@Override
		public BitVectorStruct.Builder initialElement(final IntegerStruct initialElement) {
			super.initialElement(initialElement);
			return this;
		}

		@Override
		public BitVectorStruct.Builder initialContents(final SequenceStruct initialContents) {
			super.initialContents(initialContents);
			return this;
		}

		@Override
		public BitVectorStruct.Builder adjustable(final BooleanStruct adjustable) {
			super.adjustable(adjustable);
			return this;
		}

		@Override
		public BitVectorStruct.Builder fillPointer(final IntegerStruct fillPointer) {
			super.fillPointer(fillPointer);
			return this;
		}

		@Override
		public BitVectorStruct.Builder displacedTo(final ArrayStruct<IntegerStruct> displacedTo) {
			super.displacedTo(displacedTo);
			return this;
		}

		@Override
		public BitVectorStruct.Builder displacedIndexOffset(final IntegerStruct displacedIndexOffset) {
			super.displacedIndexOffset(displacedIndexOffset);
			return this;
		}

		@Override
		public BitVectorStruct build() {
			final int sizeInt = size.intValue();
			final LispType upgradedET = ArrayStruct.upgradedArrayElementType(elementType);
			final boolean adjustableBoolean = adjustable.booleanValue();
			final Integer fillPointerInt = (fillPointer == null) ? null : fillPointer.intValue();

			if (displacedTo != null) {
				final LispType displacedToType = displacedTo.getType();
				if (displacedToType.isNotOfType(upgradedET)) {
					throw new TypeErrorException(
							"Provided displaced to " + displacedTo + " is not an array with a subtype of the upgraded-array-element-type " + upgradedET + '.');
				}

				try {
					displacedTo.rowMajorAref(displacedIndexOffset);
				} catch (final ErrorException ignore) {
					throw new ErrorException("Requested size is too large to displace to " + displacedTo + '.');
				}

				// TODO:
//				return new BitVectorStructImpl(BitVectorStruct.INSTANCE,
//				                               sizeInt,
//				                               upgradedET,
//				                               displacedTo,
//				                               displacedIndexOffset.intValue(),
//				                               adjustableBoolean,
//				                               fillPointerInt);
				return null;
			}

			final BitVectorType vectorType = (adjustableBoolean || (fillPointerInt != null))
			                                 ? BitVectorType.INSTANCE
			                                 : SimpleBitVectorType.INSTANCE;

			if (initialContents != null) {
				for (final LispStruct element : initialContents) {
					final LispType initialElementType = element.getType();
					if (initialElementType.isNotOfType(upgradedET)) {
						throw new TypeErrorException(
								"Provided element " + element + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
					}
				}

				final List<IntegerStruct> validContents
						= ArrayStruct.getValidContents(Collections.singletonList(sizeInt),
						                               upgradedET,
						                               initialContents);
				return new BitVectorStructImpl(vectorType,
				                               sizeInt,
				                               validContents,
				                               adjustableBoolean,
				                               fillPointerInt);
			} else {
				final LispType initialElementType = initialElement.getType();
				if (initialElementType.isNotOfType(upgradedET)) {
					throw new TypeErrorException(
							"Provided element " + initialElement + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
				}

				final List<IntegerStruct> contents = Stream.generate(() -> initialElement)
				                                           .limit(sizeInt)
				                                           .collect(Collectors.toList());
				return new BitVectorStructImpl(vectorType,
				                               sizeInt,
				                               contents,
				                               adjustableBoolean,
				                               fillPointerInt);
			}
		}
	}
}
