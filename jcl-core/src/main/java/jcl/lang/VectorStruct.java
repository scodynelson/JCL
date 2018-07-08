package jcl.lang;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.internal.VectorStructImpl;
import jcl.type.BitType;
import jcl.type.CharacterType;
import jcl.type.LispType;
import jcl.type.SimpleVectorType;
import jcl.type.TType;
import jcl.type.VectorType;

/**
 * The {@link VectorStruct} is the object representation of a Lisp 'vector' type.
 */
public interface VectorStruct extends ArrayStruct, SequenceStruct {

	default LispStruct svref(final IntegerStruct index) {
		final LispType type = getType();
		if (SimpleVectorType.INSTANCE.typeEquals(type)) {
			return aref(index);
		}
		throw new TypeErrorException(
				"The value " + this + " is not of the expected type " + SimpleVectorType.INSTANCE + '.');
	}

	default LispStruct setfSvref(final LispStruct newElement, final IntegerStruct index) { // TODO: check type
		final LispType type = getType();
		if (SimpleVectorType.INSTANCE.typeEquals(type)) {
			return setfAref(newElement, index);
		}
		throw new TypeErrorException(
				"The value " + this + " is not of the expected type " + SimpleVectorType.INSTANCE + '.');
	}

	/**
	 * Gets the vector's fill-pointer.
	 *
	 * @return vector's fill-pointer
	 */
	IntegerStruct fillPointer();

	/**
	 * Sets the vector's fill-pointer.
	 *
	 * @param fillPointer
	 * 		new vector fill-pointer
	 *
	 * @return the new fill-pointer value
	 */
	IntegerStruct setfFillPointer(final IntegerStruct fillPointer);

	/**
	 * Pops the element at the fill-pointer index and decreases the fill-pointer by 1.
	 *
	 * @return the element popped from the fill-pointer index
	 *
	 * @throws ErrorException
	 * 		if the vector has no fill-pointer or the fill-pointer is 0
	 */
	LispStruct vectorPop();

	/**
	 * Pushes the provided {@code element} into the current fill-pointer index.
	 *
	 * @param newElement
	 * 		the element to push into the vector
	 *
	 * @return the location of the newly added element
	 *
	 * @throws TypeErrorException
	 * 		if the vector has no fill-pointer
	 */
	LispStruct vectorPush(final LispStruct newElement);

	default IntegerStruct vectorPushExtend(final LispStruct newElement) {
		return vectorPushExtend(newElement, length());
	}

	/**
	 * Pushes the provided {@code element} into the current fill-pointer index and extends the vector to the
	 * current size of the contents plus the provided {@code extensionAmount}.
	 *
	 * @param newElement
	 * 		the element to push into the vector
	 * @param extension
	 * 		the amount to extend the vector when pushing
	 *
	 * @return the location of the newly added element
	 *
	 * @throws TypeErrorException
	 * 		if the vector has no fill-pointer or the vector is not adjustable
	 */
	IntegerStruct vectorPushExtend(final LispStruct newElement, final IntegerStruct extension);

	/*
	SEQUENCE-STRUCT
	 */

	@Override
	default IntegerStruct length() {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	@Override
	default LispStruct elt(final IntegerStruct index) {
		return aref(index);
	}

	@Override
	default LispStruct setfElt(final LispStruct newElement, final IntegerStruct index) {
		return setfAref(newElement, index);
	}

	@Override
	default VectorStruct reverse() {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	@Override
	default VectorStruct nReverse() {
		throw new UnsupportedOperationException("Not supported yet.");
	}

	/*
	LISP-STRUCT
	 */

	@Override
	default boolean equalp(final LispStruct object) {
		if (eq(object)) {
			return true;
		}
		if (object instanceof VectorStruct) {
			final VectorStruct v = (VectorStruct) object;
			if (!length().eql(v.length())) {
				return false;
			}
			for (int i = 0; i < length().toJavaInt(); i++) {
				final IntegerStruct index = IntegerStruct.toLispInteger(i);
				if (!aref(index).equalp(v.aref(index))) {
					return false;
				}
			}
			return true;
		}
		return false;
	}

	static VectorStruct.Builder builder(final IntegerStruct size) {
		return new VectorStruct.Builder(size);
	}

	final class Builder extends ArrayStruct.AbstractBuilder<VectorStruct, LispType, LispStruct> {

		private final IntegerStruct size;

		private IntegerStruct fillPointer;

		private Builder(final IntegerStruct size) {
			super(TType.INSTANCE, NILStruct.INSTANCE);
			this.size = size;
		}

		public BitVectorStruct.Builder elementType(final BitType elementType) {
			if ((initialElement != null) && !(initialElement instanceof IntegerStruct)) {
				throw new ErrorException("The value " + initialElement + " is not of the expected type BIT.");
			}
			if ((displacedTo != null) && !BitType.INSTANCE.typeEquals(displacedTo.arrayElementType())) {
				throw new ErrorException(
						"The :DISPLACED-TO array " + displacedTo + " is not of :ELEMENT-TYPE BIT");
			}
			return BitVectorStruct.builder(size)
			                      .elementType(elementType)
			                      .initialElement((IntegerStruct) initialElement)
			                      .initialContents(initialContents)
			                      .adjustable(adjustable)
			                      .fillPointer(fillPointer)
			                      .displacedTo(displacedTo)
			                      .displacedIndexOffset(displacedIndexOffset);
		}

		public StringStruct.Builder elementType(final CharacterType elementType) {
			if ((initialElement != null) && !(initialElement instanceof CharacterStruct)) {
				throw new ErrorException("The value " + initialElement + " is not of the expected type CHARACTER.");
			}
			if (!BitType.INSTANCE.typeEquals(displacedTo.arrayElementType())) {
				throw new ErrorException(
						"The :DISPLACED-TO array " + displacedTo + " is not of :ELEMENT-TYPE CHARACTER");
			}
			return StringStruct.builder(size)
			                   .elementType(elementType)
			                   .initialElement((CharacterStruct) initialElement)
			                   .initialContents(initialContents)
			                   .adjustable(adjustable)
			                   .fillPointer(fillPointer)
			                   .displacedTo(displacedTo)
			                   .displacedIndexOffset(displacedIndexOffset);
		}

		@Override
		public VectorStruct.Builder elementType(final LispType elementType) {
			this.elementType = elementType;
			return this;
		}

		@Override
		public VectorStruct.Builder initialElement(final LispStruct initialElement) {
			this.initialElement = initialElement;
			return this;
		}

		@Override
		public VectorStruct.Builder initialContents(final SequenceStruct initialContents) {
			this.initialContents = initialContents;
			return this;
		}

		@Override
		public VectorStruct.Builder adjustable(final boolean adjustable) {
			this.adjustable = adjustable;
			return this;
		}

		@Override
		public VectorStruct.Builder fillPointer(final IntegerStruct fillPointer) {
			this.fillPointer = fillPointer;
			return this;
		}

		@Override
		public VectorStruct.Builder displacedTo(final ArrayStruct displacedTo) {
			this.displacedTo = displacedTo;
			return this;
		}

		@Override
		public VectorStruct.Builder displacedIndexOffset(final IntegerStruct displacedIndexOffset) {
			this.displacedIndexOffset = displacedIndexOffset;
			return this;
		}

		@Override
		public VectorStruct build() {
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

				return new VectorStructImpl(VectorType.INSTANCE,
				                            sizeInt,
				                            upgradedET,
				                            displacedTo,
				                            displacedIndexOffset.toJavaInt(),
				                            adjustableBoolean,
				                            fillPointerInt);
			}

			final VectorType vectorType = (adjustableBoolean || (fillPointerInt != null))
			                              ? VectorType.INSTANCE
			                              : SimpleVectorType.INSTANCE;

			if (initialContents != null) {
				for (final LispStruct element : initialContents) {
					final LispType initialElementType = element.getType();
					if (!upgradedET.typeEquals(initialElementType)) {
						throw new TypeErrorException(
								"Provided element " + element + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
					}
				}

				final List<LispStruct> validContents
						= ArrayStruct.getValidContents(Collections.singletonList(sizeInt),
						                               upgradedET,
						                               initialContents);
				return new VectorStructImpl(vectorType,
				                            sizeInt,
				                            upgradedET,
				                            validContents,
				                            adjustableBoolean,
				                            fillPointerInt);
			} else {
				final LispType initialElementType = initialElement.getType();
				if (!upgradedET.typeEquals(initialElementType)) {
					throw new TypeErrorException(
							"Provided element " + initialElement + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
				}

				final List<LispStruct> contents
						= Stream.generate(() -> initialElement)
						        .limit(sizeInt)
						        .collect(Collectors.toList());
				return new VectorStructImpl(vectorType,
				                            sizeInt,
				                            upgradedET,
				                            contents,
				                            adjustableBoolean,
				                            fillPointerInt);
			}
		}
	}
}
