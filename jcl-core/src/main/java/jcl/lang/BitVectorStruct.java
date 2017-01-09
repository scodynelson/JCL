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
import jcl.type.TType;

/**
 * The {@link BitVectorStruct} is the object representation of a Lisp 'bit-vector' type.
 */
public interface BitVectorStruct extends VectorStruct<IntegerStruct>, BitArrayStruct {

	class Builder {

		private final IntegerStruct size;
		private LispType elementType = TType.INSTANCE;
		private IntegerStruct initialElement = IntegerStruct.ZERO;
		private SequenceStruct initialContents;
		private BooleanStruct adjustable = NILStruct.INSTANCE;
		private IntegerStruct fillPointer;
		private ArrayStruct<IntegerStruct> displacedTo;
		private IntegerStruct displacedIndexOffset = IntegerStruct.ZERO;

		public Builder(final IntegerStruct size) {
			this.size = size;
		}

		public BitVectorStruct.Builder elementType(final LispType elementType) {
			this.elementType = elementType;
			return this;
		}

		public BitVectorStruct.Builder initialElement(final IntegerStruct initialElement) {
			this.initialElement = initialElement;
			return this;
		}

		public BitVectorStruct.Builder initialContents(final SequenceStruct initialContents) {
			this.initialContents = initialContents;
			return this;
		}

		public BitVectorStruct.Builder adjustable(final BooleanStruct adjustable) {
			this.adjustable = adjustable;
			return this;
		}

		public BitVectorStruct.Builder fillPointer(final IntegerStruct fillPointer) {
			this.fillPointer = fillPointer;
			return this;
		}

		public BitVectorStruct.Builder displacedTo(final ArrayStruct<IntegerStruct> displacedTo) {
			this.displacedTo = displacedTo;
			return this;
		}

		public BitVectorStruct.Builder displacedIndexOffset(final IntegerStruct displacedIndexOffset) {
			this.displacedIndexOffset = displacedIndexOffset;
			return this;
		}

		public BitVectorStruct build() {
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

//				return new BitVectorStructImpl(VectorType.INSTANCE,
//				                              sizeInt,
//				                              upgradedET,
//				                              displacedTo,
//				                              displacedIndexOffset.intValue(),
//				                              adjustableBoolean,
//				                              fillPointerInt);
				// TODO:
				return null;
			}

			final BitVectorType vectorType = (adjustableBoolean || (fillPointerInt != null))
			                                 ? BitVectorType.INSTANCE
			                                 : SimpleBitVectorType.INSTANCE;

			if (initialContents != null) {
				for (final LispStruct element : initialContents) {
					final LispType initialElementType = element.getType();
					if (!initialElementType.equals(upgradedET) && !upgradedET.equals(initialElementType)) {
						throw new TypeErrorException(
								"Provided element " + element + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
					}
				}

				final List<IntegerStruct> validContents = ArrayStruct.getValidContents(
						Collections.singletonList(sizeInt),
						elementType,
						initialContents);
				return new BitVectorStructImpl(vectorType,
				                               sizeInt,
				                               validContents,
				                               adjustableBoolean,
				                               fillPointerInt);
			} else {
				final LispType initialElementType = initialElement.getType();
				if (!initialElementType.equals(upgradedET) && !upgradedET.equals(initialElementType)) {
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
