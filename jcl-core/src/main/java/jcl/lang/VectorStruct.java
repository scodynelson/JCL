package jcl.lang;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.internal.VectorStructImpl;
import jcl.type.LispType;
import jcl.type.SimpleVectorType;
import jcl.type.TType;
import jcl.type.VectorType;

/**
 * The {@link VectorStruct} is the object representation of a Lisp 'vector' type.
 *
 * @param <TYPE>
 * 		the type of the vector contents
 */
public interface VectorStruct<TYPE extends LispStruct> extends ArrayStruct<TYPE>, SequenceStruct {

	default TYPE svref(final IntegerStruct index) {
		final LispType type = getType();
		if (SimpleVectorType.INSTANCE.equals(type)) {
			return aref(index);
		}
		throw new TypeErrorException(
				"The value " + this + " is not of the expected type " + SimpleVectorType.INSTANCE + '.');
	}

	default TYPE setfSvref(final TYPE newElement, final IntegerStruct index) {
		final LispType type = getType();
		if (SimpleVectorType.INSTANCE.equals(type)) {
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
	TYPE vectorPop();

	/**
	 * Pushes the provided {@code element} into the current fill-pointer index.
	 *
	 * @param element
	 * 		the element to push into the vector
	 *
	 * @return the location of the newly added element
	 *
	 * @throws TypeErrorException
	 * 		if the vector has no fill-pointer
	 */
	LispStruct vectorPush(final TYPE element);

	/**
	 * Pushes the provided {@code element} into the current fill-pointer index and extends the vector to the
	 * current size of the contents plus the provided {@code extensionAmount}.
	 *
	 * @param element
	 * 		the element to push into the vector
	 * @param extensionAmount
	 * 		the amount to extend the vector when pushing
	 *
	 * @return the location of the newly added element
	 *
	 * @throws TypeErrorException
	 * 		if the vector has no fill-pointer or the vector is not adjustable
	 */
	IntegerStruct vectorPushExtend(final TYPE element, final IntegerStruct extensionAmount);

	class Builder<T extends LispStruct> {

		private final IntegerStruct size;
		private LispType elementType = TType.INSTANCE;
		@SuppressWarnings("unchecked")
		private T initialElement = (T) NILStruct.INSTANCE;
		private SequenceStruct initialContents;
		private BooleanStruct adjustable = NILStruct.INSTANCE;
		private IntegerStruct fillPointer;
		private ArrayStruct<T> displacedTo;
		private IntegerStruct displacedIndexOffset = IntegerStruct.ZERO;

		public Builder(final IntegerStruct size) {
			this.size = size;
		}

		public Builder<T> elementType(final LispType elementType) {
			this.elementType = elementType;
			return this;
		}

		public Builder<T> initialElement(final T initialElement) {
			this.initialElement = initialElement;
			return this;
		}

		public Builder<T> initialContents(final SequenceStruct initialContents) {
			this.initialContents = initialContents;
			return this;
		}

		public Builder<T> adjustable(final BooleanStruct adjustable) {
			this.adjustable = adjustable;
			return this;
		}

		public Builder<T> fillPointer(final IntegerStruct fillPointer) {
			this.fillPointer = fillPointer;
			return this;
		}

		public Builder<T> displacedTo(final ArrayStruct<T> displacedTo) {
			this.displacedTo = displacedTo;
			return this;
		}

		public Builder<T> displacedIndexOffset(final IntegerStruct displacedIndexOffset) {
			this.displacedIndexOffset = displacedIndexOffset;
			return this;
		}

		public VectorStruct<T> build() {
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

				return new VectorStructImpl<>(VectorType.INSTANCE,
				                              sizeInt,
				                              upgradedET,
				                              displacedTo,
				                              displacedIndexOffset.intValue(),
				                              adjustableBoolean,
				                              fillPointerInt);
			}

			final VectorType vectorType = (adjustableBoolean || (fillPointerInt != null))
			                              ? VectorType.INSTANCE
			                              : SimpleVectorType.INSTANCE;

			if (initialContents != null) {
				for (final LispStruct element : initialContents) {
					final LispType initialElementType = element.getType();
					if (!initialElementType.equals(upgradedET) && !upgradedET.equals(initialElementType)) {
						throw new TypeErrorException(
								"Provided element " + element + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
					}
				}

				final List<T> validContents = ArrayStruct.getValidContents(Collections.singletonList(sizeInt),
				                                                           elementType,
				                                                           initialContents);
				return new VectorStructImpl<>(vectorType,
				                              sizeInt,
				                              elementType,
				                              validContents,
				                              adjustableBoolean,
				                              fillPointerInt);
			} else {
				final LispType initialElementType = initialElement.getType();
				if (!initialElementType.equals(upgradedET) && !upgradedET.equals(initialElementType)) {
					throw new TypeErrorException(
							"Provided element " + initialElement + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
				}

				final List<T> contents = Stream.generate(() -> initialElement)
				                               .limit(sizeInt)
				                               .collect(Collectors.toList());
				return new VectorStructImpl<>(vectorType,
				                              sizeInt,
				                              elementType,
				                              contents,
				                              adjustableBoolean,
				                              fillPointerInt);
			}
		}
	}
}
