package jcl.lang;

import java.util.Collections;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.type.LispType;
import jcl.type.SimpleArrayType;
import jcl.type.SimpleVectorType;
import jcl.type.TType;
import jcl.type.VectorType;

/**
 * The {@link BitArrayStruct} is the object representation of a Lisp 'bit-array' type.
 */
public interface BitArrayStruct extends ArrayStruct<IntegerStruct> {

	default IntegerStruct bit(final IntegerStruct... subscripts) {
		return aref(subscripts);
	}

	default IntegerStruct setfBit(final IntegerStruct newElement, final IntegerStruct... subscripts) {
		return setfAref(newElement, subscripts);
	}

	default IntegerStruct sbit(final IntegerStruct... subscripts) {
		final LispType type = getType();
		if (SimpleArrayType.INSTANCE.equals(type)) {
			return aref(subscripts);
		}
		throw new TypeErrorException(
				"The value " + this + " is not of the expected type " + SimpleArrayType.INSTANCE + '.');
	}

	default IntegerStruct setfSbit(final IntegerStruct newElement, final IntegerStruct... subscripts) {
		final LispType type = getType();
		if (SimpleArrayType.INSTANCE.equals(type)) {
			return setfAref(newElement, subscripts);
		}
		throw new TypeErrorException(
				"The value " + this + " is not of the expected type " + SimpleArrayType.INSTANCE + '.');
	}

	default BitArrayStruct bitAnd(final BitArrayStruct bitArray2, final LispStruct optArg) {
		return bitFunction(bitArray2, optArg, bitLogicContents -> {
			final int index = bitLogicContents.index;
			return ((bitLogicContents.contents1.get(index).intValue() == 1)
					&& (bitLogicContents.contents2.get(index).intValue() == 1))
			       ? IntegerStruct.ONE
			       : IntegerStruct.ZERO;
		});
	}

	default BitArrayStruct bitAndC1(final BitArrayStruct bitArray2, final LispStruct optArg) {
		return bitFunction(bitArray2, optArg, bitLogicContents -> {
			final int index = bitLogicContents.index;
			return ((bitLogicContents.contents1.get(index).intValue() != 1)
					&& (bitLogicContents.contents2.get(index).intValue() == 1))
			       ? IntegerStruct.ONE
			       : IntegerStruct.ZERO;
		});
	}

	default BitArrayStruct bitAndC2(final BitArrayStruct bitArray2, final LispStruct optArg) {
		return bitFunction(bitArray2, optArg, bitLogicContents -> {
			final int index = bitLogicContents.index;
			return ((bitLogicContents.contents1.get(index).intValue() == 1)
					&& (bitLogicContents.contents2.get(index).intValue() != 1))
			       ? IntegerStruct.ONE
			       : IntegerStruct.ZERO;
		});
	}

	default BitArrayStruct bitEqv(final BitArrayStruct bitArray2, final LispStruct optArg) {
		return bitFunction(bitArray2, optArg, bitLogicContents -> {
			final int index = bitLogicContents.index;
			return ((bitLogicContents.contents1.get(index).intValue() == 1)
					== (bitLogicContents.contents2.get(index).intValue() == 1))
			       ? IntegerStruct.ONE
			       : IntegerStruct.ZERO;
		});
	}

	default BitArrayStruct bitIor(final BitArrayStruct bitArray2, final LispStruct optArg) {
		return bitFunction(bitArray2, optArg, bitLogicContents -> {
			final int index = bitLogicContents.index;
			return ((bitLogicContents.contents1.get(index).intValue() == 1)
					|| (bitLogicContents.contents2.get(index).intValue() == 1))
			       ? IntegerStruct.ONE
			       : IntegerStruct.ZERO;
		});
	}

	default BitArrayStruct bitNand(final BitArrayStruct bitArray2, final LispStruct optArg) {
		return bitFunction(bitArray2, optArg, bitLogicContents -> {
			final int index = bitLogicContents.index;
			return ((bitLogicContents.contents1.get(index).intValue() == 1)
					&& (bitLogicContents.contents2.get(index).intValue() == 1))
			       ? IntegerStruct.ZERO
			       : IntegerStruct.ONE;
		});
	}

	default BitArrayStruct bitNor(final BitArrayStruct bitArray2, final LispStruct optArg) {
		return bitFunction(bitArray2, optArg, bitLogicContents -> {
			final int index = bitLogicContents.index;
			return ((bitLogicContents.contents1.get(index).intValue() == 1)
					|| (bitLogicContents.contents2.get(index).intValue() == 1))
			       ? IntegerStruct.ZERO
			       : IntegerStruct.ONE;
		});
	}

	default BitArrayStruct bitOrC1(final BitArrayStruct bitArray2, final LispStruct optArg) {
		return bitFunction(bitArray2, optArg, bitLogicContents -> {
			final int index = bitLogicContents.index;
			return ((bitLogicContents.contents1.get(index).intValue() == 0)
					|| (bitLogicContents.contents2.get(index).intValue() == 1))
			       ? IntegerStruct.ONE
			       : IntegerStruct.ZERO;
		});
	}

	default BitArrayStruct bitOrC2(final BitArrayStruct bitArray2, final LispStruct optArg) {
		return bitFunction(bitArray2, optArg, bitLogicContents -> {
			final int index = bitLogicContents.index;
			return ((bitLogicContents.contents1.get(index).intValue() == 1)
					|| (bitLogicContents.contents2.get(index).intValue() == 0))
			       ? IntegerStruct.ONE
			       : IntegerStruct.ZERO;
		});
	}

	default BitArrayStruct bitXor(final BitArrayStruct bitArray2, final LispStruct optArg) {
		return bitFunction(bitArray2, optArg, bitLogicContents -> {
			final int index = bitLogicContents.index;
			final IntegerStruct newBit;
			final int bit1 = bitLogicContents.contents1.get(index).intValue();
			final int bit2 = bitLogicContents.contents2.get(index).intValue();
			if (((bit1 == 0) && (bit2 == 1))
					|| ((bit1 == 1) && (bit2 == 0))) {
				newBit = IntegerStruct.ONE;
			} else {
				newBit = IntegerStruct.ZERO;
			}
			return newBit;
		});
	}

	default BitArrayStruct bitNot(final LispStruct optArg) {
		final List<Integer> dimensions1 = getDimensions();

		final BitArrayStruct bitArrayToUpdate = getBitArrayToUpdate(optArg, dimensions1);

		final List<IntegerStruct> contents1 = getContents();
		final List<IntegerStruct> contentsToUpdate = bitArrayToUpdate.getContents();

		final int totalSize = arrayTotalSize().intValue();
		for (int i = 0; i < totalSize; i++) {
			final IntegerStruct newBit = (contents1.get(i).intValue() == 0) ? IntegerStruct.ONE : IntegerStruct.ZERO;
			contentsToUpdate.set(i, newBit);
		}

		return bitArrayToUpdate;
	}

	// TODO: Make 'private' when we have Java 9
	default BitArrayStruct bitFunction(final BitArrayStruct bitArray2, final LispStruct optArg,
	                                   final Function<BitLogicContents, IntegerStruct> bitLogicFunction) {
		final List<Integer> dimensions1 = getDimensions();
		final List<Integer> dimensions2 = bitArray2.getDimensions();
		if (!dimensions1.equals(dimensions2)) {
			throw new ErrorException(this + " and " + bitArray2 + " do not have the same dimensions.");
		}

		final BitArrayStruct bitArrayToUpdate = getBitArrayToUpdate(optArg, dimensions1);

		final List<IntegerStruct> contents1 = getContents();
		final List<IntegerStruct> contents2 = bitArray2.getContents();
		final List<IntegerStruct> contentsToUpdate = bitArrayToUpdate.getContents();

		final int totalSize = arrayTotalSize().intValue();
		for (int i = 0; i < totalSize; i++) {
			final IntegerStruct newBit = bitLogicFunction.apply(new BitLogicContents(contents1, contents2, i));
			contentsToUpdate.set(i, newBit);
		}

		return bitArrayToUpdate;
	}

	// TODO: Make 'private' when we have Java 9
	default BitArrayStruct getBitArrayToUpdate(final LispStruct optArg, final List<Integer> dimensions1) {
		final BitArrayStruct bitArrayToUpdate;
		if (TStruct.INSTANCE.equals(optArg)) {
			bitArrayToUpdate = this;
		} else if (NILStruct.INSTANCE.equals(optArg)) {
			bitArrayToUpdate = copyBitArray();
		} else if (optArg instanceof BitArrayStruct) {
			bitArrayToUpdate = (BitArrayStruct) optArg;
			final List<Integer> optArgDimensions = bitArrayToUpdate.getDimensions();
			if (!dimensions1.equals(optArgDimensions)) {
				throw new ErrorException(this + " and " + optArg + " do not have the same dimensions.");
			}
		} else {
			throw new TypeErrorException(
					"The value " + optArg + " is not of the expected type T, NIL, or BIT-ARRAY.");
		}
		return bitArrayToUpdate;
	}

	BitArrayStruct copyBitArray();

	class Builder {

		private final List<IntegerStruct> dimensions;
		private LispType elementType = TType.INSTANCE;
		private IntegerStruct initialElement = IntegerStruct.ZERO;
		private SequenceStruct initialContents;
		private BooleanStruct adjustable = NILStruct.INSTANCE;
		private IntegerStruct fillPointer;
		private ArrayStruct<IntegerStruct> displacedTo;
		private IntegerStruct displacedIndexOffset = IntegerStruct.ZERO;

		public Builder(final List<IntegerStruct> dimensions) {
			this.dimensions = dimensions;
		}

		public BitArrayStruct.Builder elementType(final LispType elementType) {
			this.elementType = elementType;
			return this;
		}

		public BitArrayStruct.Builder initialElement(final IntegerStruct initialElement) {
			this.initialElement = initialElement;
			return this;
		}

		public BitArrayStruct.Builder initialContents(final SequenceStruct initialContents) {
			this.initialContents = initialContents;
			return this;
		}

		public BitArrayStruct.Builder adjustable(final BooleanStruct adjustable) {
			this.adjustable = adjustable;
			return this;
		}

		public BitArrayStruct.Builder fillPointer(final IntegerStruct fillPointer) {
			this.fillPointer = fillPointer;
			return this;
		}

		public BitArrayStruct.Builder displacedTo(final ArrayStruct<IntegerStruct> displacedTo) {
			this.displacedTo = displacedTo;
			return this;
		}

		public BitArrayStruct.Builder displacedIndexOffset(final IntegerStruct displacedIndexOffset) {
			this.displacedIndexOffset = displacedIndexOffset;
			return this;
		}

		public BitArrayStruct build() {
			if (dimensions.size() == 1) {
				return new BitVectorStruct.Builder(dimensions.get(0))
						.elementType(elementType)
						.initialElement(initialElement)
						.initialContents(initialContents)
						.adjustable(adjustable)
						.fillPointer(fillPointer)
						.displacedTo(displacedTo)
						.displacedIndexOffset(displacedIndexOffset)
						.build();
			}

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

//				return new VectorStructImpl<>(VectorType.INSTANCE,
//				                              sizeInt,
//				                              upgradedET,
//				                              displacedTo,
//				                              displacedIndexOffset.intValue(),
//				                              adjustableBoolean,
//				                              fillPointerInt);
				return null;
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

				final List<IntegerStruct> validContents = ArrayStruct.getValidContents(Collections.singletonList(0),
				                                                                       // TODO
				                                                                       elementType,
				                                                                       initialContents);
//				return new VectorStructImpl<>(vectorType,
//				                              sizeInt,
//				                              elementType,
//				                              validContents,
//				                              adjustableBoolean,
//				                              fillPointerInt);
				return null;
			} else {
				final LispType initialElementType = initialElement.getType();
				if (!initialElementType.equals(upgradedET) && !upgradedET.equals(initialElementType)) {
					throw new TypeErrorException(
							"Provided element " + initialElement + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
				}

				final List<IntegerStruct> contents = Stream.generate(() -> initialElement)
				                                           .limit(0) // TODO
				                                           .collect(Collectors.toList());
//				return new VectorStructImpl<>(vectorType,
//				                              sizeInt,
//				                              elementType,
//				                              contents,
//				                              adjustableBoolean,
//				                              fillPointerInt);
				return null;
			}
		}
	}
}
