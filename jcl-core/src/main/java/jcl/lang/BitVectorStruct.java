package jcl.lang;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.internal.BitVectorStructImpl;
import jcl.type.BitType;
import jcl.type.BitVectorType;
import jcl.type.LispType;
import jcl.type.SimpleBitVectorType;

/**
 * The {@link BitVectorStruct} is the object representation of a Lisp 'bit-vector' type.
 */
public interface BitVectorStruct extends VectorStruct, BitArrayStruct {

	static BitVectorStruct.Builder builder(final IntegerStruct size) {
		return new BitVectorStruct.Builder(size);
	}

	/**
	 * Gets the array contents.
	 *
	 * @return array contents
	 */
	List<IntegerStruct> getBVContents();

	@Override
	default boolean equal(final LispStruct object) {
		if (eq(object)) {
			return true;
		}
		if (object instanceof BitVectorStruct) {
			final BitVectorStruct v = (BitVectorStruct) object;
			if (!length().eql(v.length())) {
				return false;
			}
			for (int i = 0; i < length().toJavaInt(); i++) {
				final IntegerStruct index = IntegerStruct.toLispInteger(i);
				if (!bit(index).equal(v.bit(index))) {
					return false;
				}
			}
			return true;
		}
		return false;
	}

	@Override
	default boolean equalp(final LispStruct object) {
		if (eq(object)) {
			return true;
		}
		if (object instanceof BitVectorStruct) {
			final BitVectorStruct v = (BitVectorStruct) object;
			if (!length().eql(v.length())) {
				return false;
			}
			for (int i = 0; i < length().toJavaInt(); i++) {
				final IntegerStruct index = IntegerStruct.toLispInteger(i);
				if (!bit(index).equalp(v.bit(index))) {
					return false;
				}
			}
			return true;
		}
		if (object instanceof StringStruct) {
			return false;
		}
		if (object instanceof VectorStruct) {
			return object.equalp(this);
		}
		return false;
	}

	final class Builder extends ArrayStruct.AbstractBuilder<BitVectorStruct, BitType, IntegerStruct> {

		private final IntegerStruct size;
		private IntegerStruct fillPointer;

		private Builder(final IntegerStruct size) {
			super(BitType.INSTANCE, IntegerStruct.ZERO);
			this.size = size;
		}

		@Override
		public BitVectorStruct.Builder elementType(final BitType elementType) {
			this.elementType = elementType;
			return this;
		}

		@Override
		public BitVectorStruct.Builder initialElement(final IntegerStruct initialElement) {
			this.initialElement = initialElement;
			return this;
		}

		@Override
		public BitVectorStruct.Builder initialContents(final SequenceStruct initialContents) {
			this.initialContents = initialContents;
			return this;
		}

		@Override
		public BitVectorStruct.Builder adjustable(final boolean adjustable) {
			this.adjustable = adjustable;
			return this;
		}

		@Override
		public BitVectorStruct.Builder fillPointer(final IntegerStruct fillPointer) {
			this.fillPointer = fillPointer;
			return this;
		}

		@Override
		public BitVectorStruct.Builder displacedTo(final ArrayStruct displacedTo) {
			this.displacedTo = displacedTo;
			return this;
		}

		@Override
		public BitVectorStruct.Builder displacedIndexOffset(final IntegerStruct displacedIndexOffset) {
			this.displacedIndexOffset = displacedIndexOffset;
			return this;
		}

		@Override
		public BitVectorStruct build() {
			final int sizeInt = size.toJavaInt();
			final LispType upgradedET = ArrayStruct.upgradedArrayElementType(elementType);
			final boolean adjustableBoolean = adjustable;
			final Integer fillPointerInt = (fillPointer == null) ? null : fillPointer.toJavaInt();

			if (displacedTo != null) {
				final LispType displacedToType = displacedTo.getType();
				if (!upgradedET.typeEquals(displacedToType)) {
					throw new TypeErrorException(
							"Provided displaced to " + displacedTo + " is not an array with a subtype of the upgraded-array-element-type " + upgradedET + '.');
				}

				try {
					displacedTo.rowMajorAref(displacedIndexOffset);
				} catch (final ErrorException ignored) {
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
					if (!upgradedET.typeEquals(initialElementType)) {
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
				if (!upgradedET.typeEquals(initialElementType)) {
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
