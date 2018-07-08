package jcl.lang;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.SimpleErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.internal.MultiArrayStructImpl;
import jcl.lang.internal.NILArrayStructImpl;
import jcl.type.ArrayType;
import jcl.type.BaseCharType;
import jcl.type.BitType;
import jcl.type.CharacterType;
import jcl.type.ExtendedCharType;
import jcl.type.LispType;
import jcl.type.NILType;
import jcl.type.SimpleArrayType;
import jcl.type.StandardCharType;
import jcl.type.TType;

/**
 * The {@link ArrayStruct} is the object representation of a Lisp 'array' type.
 */
public interface ArrayStruct extends LispStruct {

	default ArrayStruct adjustArray(final AdjustArrayContext context) {
		return null;
	}

	ArrayStruct adjustArray(final List<IntegerStruct> dimensions, final LispType elementType,
	                        final LispStruct initialElement, final IntegerStruct fillPointer);

	ArrayStruct adjustArray(final List<IntegerStruct> dimensions, final LispType elementType,
	                        final SequenceStruct initialContents, final IntegerStruct fillPointer);

	ArrayStruct adjustArray(final List<IntegerStruct> dimensions, final LispType elementType,
	                        final IntegerStruct fillPointer, final ArrayStruct displacedTo,
	                        final IntegerStruct displacedIndexOffset);

	boolean adjustableArrayP();

	LispStruct aref(final IntegerStruct... subscripts);

	LispStruct setfAref(final LispStruct newElement, final IntegerStruct... subscripts);

	IntegerStruct arrayDimension(final IntegerStruct axisNumber);

	ListStruct arrayDimensions();

	/**
	 * Gets the array elementType.
	 *
	 * @return array elementType
	 */
	LispType arrayElementType();

	boolean arrayHasFillPointerP();

	ValuesStruct arrayDisplacement();

	boolean arrayInBoundsP(final IntegerStruct... subscripts);

	/**
	 * Gets the array rank.
	 *
	 * @return array rank
	 */
	IntegerStruct arrayRank();

	IntegerStruct arrayRowMajorIndex(final IntegerStruct... subscripts);

	/**
	 * Gets the array's total size.
	 *
	 * @return array's total size
	 */
	IntegerStruct arrayTotalSize();

	LispStruct rowMajorAref(final IntegerStruct index);

	LispStruct setfRowMajorAref(final LispStruct newElement, final IntegerStruct index);

	static LispType upgradedArrayElementType(final LispType type) {
		if (CharacterType.INSTANCE.eq(type)
				|| BaseCharType.INSTANCE.eq(type)
				|| StandardCharType.INSTANCE.eq(type)
				|| ExtendedCharType.INSTANCE.eq(type)) {
			return CharacterType.INSTANCE;
		}
		if (BitType.INSTANCE.eq(type)) {
			return BitType.INSTANCE;
		}
		if (NILType.INSTANCE.eq(type)) {
			return NILType.INSTANCE;
		}
		return TType.INSTANCE;
	}

	@Override
	default boolean equalp(final LispStruct object) {
		if (eq(object)) {
			return true;
		}
		if (object instanceof ArrayStruct) {
			final ArrayStruct a = (ArrayStruct) object;
			if (!arrayRank().eql(a.arrayRank())) {
				return false;
			}
			for (int i = 0; i < arrayRank().toJavaInt(); i++) {
				final IntegerStruct axisNumber = IntegerStruct.toLispInteger(i);
				if (!arrayDimension(axisNumber).eql(a.arrayDimension(axisNumber))) {
					return false;
				}
			}
			for (int i = 0; i < arrayTotalSize().toJavaInt(); i++) {
				final IntegerStruct index = IntegerStruct.toLispInteger(i);
				if (!rowMajorAref(index).equalp(a.rowMajorAref(index))) {
					return false;
				}
			}
			return true;
		}
		return false;
	}

	// =================

	/**
	 * Gets the array contents.
	 *
	 * @return array contents
	 */
	List<LispStruct> getContents();

	/**
	 * Gets the array dimensions.
	 *
	 * @return array dimensions
	 */
	List<Integer> getDimensions();

	/**
	 * Determines if the provided {@code dimensionsToCheck} and {@code elementTypeToCheck} are valid for the provided
	 * {@code contentsToCheck}.
	 *
	 * @param dimensions
	 * 		the array dimensions to check
	 * @param elementType
	 * 		the array elementType to check
	 * @param initialContents
	 * 		the array contents to check
	 * @param <TYPE>
	 * 		the type of the array contents
	 *
	 * @return the valid array contents
	 */
	static <TYPE extends LispStruct> List<TYPE> getValidContents(final List<Integer> dimensions,
	                                                             final LispType elementType,
	                                                             final SequenceStruct initialContents) {
		final int numberOfDimensions = dimensions.size();
		if (numberOfDimensions == 0) {
			return Collections.emptyList();
		}

		if (numberOfDimensions == 1) {
			final int dimension = dimensions.get(0);
			if (initialContents.length().toJavaInt() == dimension) {
				return getValidContents(elementType, initialContents);
			} else {
				throw new SimpleErrorException(
						initialContents + " doesn't match array dimensions of #<" + elementType + ' ' + dimension + ">.");
			}
		}

		final List<TYPE> validContents = new ArrayList<>();

		final int dimension = dimensions.get(0);
		if (initialContents.length().toJavaInt() == dimension) {
			final List<Integer> subDimension = dimensions.subList(1, numberOfDimensions);

			for (final LispStruct contentToCheck : initialContents) {
				if (!(contentToCheck instanceof SequenceStruct)) {
					throw new SimpleErrorException(
							initialContents + " doesn't match array dimensions of #<" + elementType + ' ' + dimension + ">.");
				}

				final SequenceStruct subContents = (SequenceStruct) contentToCheck;
				final List<TYPE> validSubContents = getValidContents(subDimension, elementType, subContents);
				validContents.addAll(validSubContents);
			}
		} else {
			throw new SimpleErrorException(
					initialContents + " doesn't match array dimensions of #<" + elementType + ' ' + dimension + ">.");
		}

		return validContents;
	}

	@SuppressWarnings("unchecked")
	static <TYPE extends LispStruct> List<TYPE> getValidContents(final LispType elementType,
	                                                             final SequenceStruct initialContents) {
		final List<TYPE> validContents = new ArrayList<>();

		for (final LispStruct current : initialContents) {
			final LispType currentType = current.getType();
			if (currentType.isNotOfType(elementType)) {
				throw new TypeErrorException(
						"Provided element " + current + " is not a subtype of the provided elementType " + elementType + '.');
			} else {
				validContents.add((TYPE) current);
			}
		}
		return validContents;
	}

	static VectorStruct.Builder builder(final IntegerStruct size) {
		return VectorStruct.builder(size);
	}

	static ArrayStruct.Builder builder(final IntegerStruct... dimensions) {
		return new ArrayStruct.Builder(dimensions);
	}

	final class Builder extends AbstractBuilder<ArrayStruct, LispType, LispStruct> {

		private final IntegerStruct[] dimensions;

		private Builder(final IntegerStruct... dimensions) {
			super(TType.INSTANCE, NILStruct.INSTANCE);
			this.dimensions = dimensions;
		}

		@Override
		public ArrayStruct.Builder elementType(final LispType elementType) {
			this.elementType = elementType;
			return this;
		}

		public BitArrayStruct.Builder elementType(final BitType elementType) {
			if ((initialElement != null) && !(initialElement instanceof IntegerStruct)) {
				throw new ErrorException("The value " + initialElement + " is not of the expected type BIT.");
			}
			if (!BitType.INSTANCE.typeEquals(displacedTo.arrayElementType())) {
				throw new ErrorException(
						"The :DISPLACED-TO array " + displacedTo + " is not of :ELEMENT-TYPE BIT");
			}
			return BitArrayStruct.builder(dimensions)
			                     .elementType(elementType)
			                     .initialElement((IntegerStruct) initialElement)
			                     .initialContents(initialContents)
			                     .adjustable(adjustable)
			                     .displacedTo(displacedTo)
			                     .displacedIndexOffset(displacedIndexOffset);
		}

		@Override
		public ArrayStruct.Builder initialElement(final LispStruct initialElement) {
			this.initialElement = initialElement;
			return this;
		}

		@Override
		public ArrayStruct.Builder initialContents(final SequenceStruct initialContents) {
			this.initialContents = initialContents;
			return this;
		}

		@Override
		public ArrayStruct.Builder adjustable(final boolean adjustable) {
			this.adjustable = adjustable;
			return this;
		}

		@Override
		public ArrayStruct.Builder fillPointer(final IntegerStruct fillPointer) {
			throw new ErrorException("Non-vector arrays cannot adjust fill-pointer.");
		}

		@Override
		public ArrayStruct.Builder displacedTo(final ArrayStruct displacedTo) {
			this.displacedTo = displacedTo;
			return this;
		}

		@Override
		public ArrayStruct.Builder displacedIndexOffset(final IntegerStruct displacedIndexOffset) {
			this.displacedIndexOffset = displacedIndexOffset;
			return this;
		}

		@Override
		public ArrayStruct build() {
			final LispType upgradedET = upgradedArrayElementType(elementType);
			final boolean adjustableBoolean = adjustable;

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

				if (dimensions.length == 0) {
					return new NILArrayStructImpl(ArrayType.INSTANCE,
					                              upgradedET,
					                              displacedTo,
					                              displacedIndexOffset.toJavaInt(),
					                              adjustableBoolean);
				}

				final List<Integer> dimensionInts = Arrays.stream(dimensions)
				                                          .map(IntegerStruct::toJavaInt)
				                                          .collect(Collectors.toList());
				return new MultiArrayStructImpl(ArrayType.INSTANCE,
				                                dimensionInts,
				                                upgradedET,
				                                displacedTo,
				                                displacedIndexOffset.toJavaInt(),
				                                adjustableBoolean);
			}

			final ArrayType arrayType = adjustableBoolean
			                            ? ArrayType.INSTANCE
			                            : SimpleArrayType.INSTANCE;

			if (initialContents != null) {
				for (final LispStruct element : initialContents) {
					final LispType initialElementType = element.getType();
					if (!upgradedET.typeEquals(initialElementType)) {
						throw new TypeErrorException(
								"Provided element " + element + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
					}
				}

				if (dimensions.length == 0) {
					return new NILArrayStructImpl(arrayType,
					                              upgradedET,
					                              initialContents,
					                              adjustableBoolean);
				}

				final List<Integer> dimensionInts = Arrays.stream(dimensions)
				                                          .map(IntegerStruct::toJavaInt)
				                                          .collect(Collectors.toList());
				final List<LispStruct> validContents
						= getValidContents(dimensionInts,
						                   upgradedET,
						                   initialContents);
				return new MultiArrayStructImpl(arrayType,
				                                dimensionInts,
				                                upgradedET,
				                                validContents,
				                                adjustableBoolean);
			} else {
				final LispType initialElementType = initialElement.getType();
				if (!upgradedET.typeEquals(initialElementType)) {
					throw new TypeErrorException(
							"Provided element " + initialElement + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
				}

				if (dimensions.length == 0) {
					return new NILArrayStructImpl(arrayType,
					                              upgradedET,
					                              initialElement,
					                              adjustableBoolean);
				}

				final List<Integer> dimensionInts = Arrays.stream(dimensions)
				                                          .map(IntegerStruct::toJavaInt)
				                                          .collect(Collectors.toList());
				final int totalSize = dimensionInts.stream()
				                                   .mapToInt(Integer::intValue)
				                                   .reduce(1, (x, y) -> x * y);
				final List<LispStruct> contents
						= Stream.generate(() -> initialElement)
						        .limit(totalSize)
						        .collect(Collectors.toList());
				return new MultiArrayStructImpl(arrayType,
				                                dimensionInts,
				                                upgradedET,
				                                contents,
				                                adjustableBoolean);
			}
		}
	}

	abstract class AbstractBuilder<AS extends ArrayStruct, ET extends LispType, IE extends LispStruct> {

		protected ET elementType;
		protected IE initialElement;
		protected SequenceStruct initialContents;
		protected boolean adjustable;
		protected ArrayStruct displacedTo;
		protected IntegerStruct displacedIndexOffset = IntegerStruct.ZERO;

		AbstractBuilder(final ET elementType, final IE initialElement) {
			this.elementType = elementType;
			this.initialElement = initialElement;
		}

		public abstract ArrayStruct.AbstractBuilder<AS, ET, IE> elementType(
				final ET elementType);

		public abstract ArrayStruct.AbstractBuilder<AS, ET, IE> initialElement(
				final IE initialElement);

		public abstract ArrayStruct.AbstractBuilder<AS, ET, IE> initialContents(
				final SequenceStruct initialContents);

		public abstract ArrayStruct.AbstractBuilder<AS, ET, IE> adjustable(
				final boolean adjustable);

		public abstract ArrayStruct.AbstractBuilder<AS, ET, IE> fillPointer(
				final IntegerStruct fillPointer);

		public abstract ArrayStruct.AbstractBuilder<AS, ET, IE> displacedTo(
				final ArrayStruct displacedTo);

		public abstract ArrayStruct.AbstractBuilder<AS, ET, IE> displacedIndexOffset(
				final IntegerStruct displacedIndexOffset);

		public abstract AS build();
	}
}
