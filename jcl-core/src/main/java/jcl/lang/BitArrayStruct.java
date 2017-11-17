package jcl.lang;

import java.util.Arrays;
import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.lang.internal.MultiBitArrayStructImpl;
import jcl.lang.internal.NILBitArrayStructImpl;
import jcl.type.ArrayType;
import jcl.type.BitType;
import jcl.type.LispType;
import jcl.type.SimpleArrayType;

/**
 * The {@link BitArrayStruct} is the object representation of a Lisp 'bit-array' type.
 */
public interface BitArrayStruct extends ArrayStruct {

	default IntegerStruct bit(final IntegerStruct... subscripts) {
		return (IntegerStruct) aref(subscripts); // TODO
	}

	default IntegerStruct setfBit(final IntegerStruct newElement, final IntegerStruct... subscripts) {
		return (IntegerStruct) setfAref(newElement, subscripts); // TODO
	}

	default IntegerStruct sbit(final IntegerStruct... subscripts) {
		final LispType type = getType();
		if (SimpleArrayType.INSTANCE.isOfType(type)) {
			return (IntegerStruct) aref(subscripts); // TODO
		}
		throw new TypeErrorException(
				"The value " + this + " is not of the expected type " + SimpleArrayType.INSTANCE + '.');
	}

	default IntegerStruct setfSbit(final IntegerStruct newElement, final IntegerStruct... subscripts) {
		final LispType type = getType();
		if (SimpleArrayType.INSTANCE.isOfType(type)) {
			return (IntegerStruct) setfAref(newElement, subscripts); // TODO
		}
		throw new TypeErrorException(
				"The value " + this + " is not of the expected type " + SimpleArrayType.INSTANCE + '.');
	}

	default BitArrayStruct bitAnd(final BitArrayStruct bitArray2, final LispStruct optArg) {
		return bitFunction(bitArray2, optArg, bitLogicContents -> {
			final int index = bitLogicContents.index;
			return ((bitLogicContents.contents1.get(index).toJavaInt() == 1)
					&& (bitLogicContents.contents2.get(index).toJavaInt() == 1))
			       ? IntegerStruct.ONE
			       : IntegerStruct.ZERO;
		});
	}

	default BitArrayStruct bitAndC1(final BitArrayStruct bitArray2, final LispStruct optArg) {
		return bitFunction(bitArray2, optArg, bitLogicContents -> {
			final int index = bitLogicContents.index;
			return ((bitLogicContents.contents1.get(index).toJavaInt() != 1)
					&& (bitLogicContents.contents2.get(index).toJavaInt() == 1))
			       ? IntegerStruct.ONE
			       : IntegerStruct.ZERO;
		});
	}

	default BitArrayStruct bitAndC2(final BitArrayStruct bitArray2, final LispStruct optArg) {
		return bitFunction(bitArray2, optArg, bitLogicContents -> {
			final int index = bitLogicContents.index;
			return ((bitLogicContents.contents1.get(index).toJavaInt() == 1)
					&& (bitLogicContents.contents2.get(index).toJavaInt() != 1))
			       ? IntegerStruct.ONE
			       : IntegerStruct.ZERO;
		});
	}

	default BitArrayStruct bitEqv(final BitArrayStruct bitArray2, final LispStruct optArg) {
		return bitFunction(bitArray2, optArg, bitLogicContents -> {
			final int index = bitLogicContents.index;
			return ((bitLogicContents.contents1.get(index).toJavaInt() == 1)
					== (bitLogicContents.contents2.get(index).toJavaInt() == 1))
			       ? IntegerStruct.ONE
			       : IntegerStruct.ZERO;
		});
	}

	default BitArrayStruct bitIor(final BitArrayStruct bitArray2, final LispStruct optArg) {
		return bitFunction(bitArray2, optArg, bitLogicContents -> {
			final int index = bitLogicContents.index;
			return ((bitLogicContents.contents1.get(index).toJavaInt() == 1)
					|| (bitLogicContents.contents2.get(index).toJavaInt() == 1))
			       ? IntegerStruct.ONE
			       : IntegerStruct.ZERO;
		});
	}

	default BitArrayStruct bitNand(final BitArrayStruct bitArray2, final LispStruct optArg) {
		return bitFunction(bitArray2, optArg, bitLogicContents -> {
			final int index = bitLogicContents.index;
			return ((bitLogicContents.contents1.get(index).toJavaInt() == 1)
					&& (bitLogicContents.contents2.get(index).toJavaInt() == 1))
			       ? IntegerStruct.ZERO
			       : IntegerStruct.ONE;
		});
	}

	default BitArrayStruct bitNor(final BitArrayStruct bitArray2, final LispStruct optArg) {
		return bitFunction(bitArray2, optArg, bitLogicContents -> {
			final int index = bitLogicContents.index;
			return ((bitLogicContents.contents1.get(index).toJavaInt() == 1)
					|| (bitLogicContents.contents2.get(index).toJavaInt() == 1))
			       ? IntegerStruct.ZERO
			       : IntegerStruct.ONE;
		});
	}

	default BitArrayStruct bitOrC1(final BitArrayStruct bitArray2, final LispStruct optArg) {
		return bitFunction(bitArray2, optArg, bitLogicContents -> {
			final int index = bitLogicContents.index;
			return ((bitLogicContents.contents1.get(index).toJavaInt() == 0)
					|| (bitLogicContents.contents2.get(index).toJavaInt() == 1))
			       ? IntegerStruct.ONE
			       : IntegerStruct.ZERO;
		});
	}

	default BitArrayStruct bitOrC2(final BitArrayStruct bitArray2, final LispStruct optArg) {
		return bitFunction(bitArray2, optArg, bitLogicContents -> {
			final int index = bitLogicContents.index;
			return ((bitLogicContents.contents1.get(index).toJavaInt() == 1)
					|| (bitLogicContents.contents2.get(index).toJavaInt() == 0))
			       ? IntegerStruct.ONE
			       : IntegerStruct.ZERO;
		});
	}

	default BitArrayStruct bitXor(final BitArrayStruct bitArray2, final LispStruct optArg) {
		return bitFunction(bitArray2, optArg, bitLogicContents -> {
			final int index = bitLogicContents.index;
			final IntegerStruct newBit;
			final int bit1 = bitLogicContents.contents1.get(index).toJavaInt();
			final int bit2 = bitLogicContents.contents2.get(index).toJavaInt();
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

		final List<IntegerStruct> contents1 = null; //TODO getContents();
		final List<IntegerStruct> contentsToUpdate = null; //TODO bitArrayToUpdate.getContents();

		final int totalSize = arrayTotalSize().toJavaInt();
		for (int i = 0; i < totalSize; i++) {
			final IntegerStruct newBit = (contents1.get(i).toJavaInt() == 0) ? IntegerStruct.ONE : IntegerStruct.ZERO;
			contentsToUpdate.set(i, newBit);
		}

		return bitArrayToUpdate;
	}

	// TODO: Make 'private' when we have Java 9
	default BitArrayStruct bitFunction(final BitArrayStruct bitArray2, final LispStruct optArg,
	                                   final Function<BitLogicContents, IntegerStruct> bitLogicFunction) {
		final List<Integer> dimensions1 = getDimensions();
		final List<Integer> dimensions2 = bitArray2.getDimensions();
		// TODO: check this dimension equality
		if (!dimensions1.equals(dimensions2)) {
			throw new ErrorException(this + " and " + bitArray2 + " do not have the same dimensions.");
		}

		final BitArrayStruct bitArrayToUpdate = getBitArrayToUpdate(optArg, dimensions1);

		final List<IntegerStruct> contents1 = null; //TODO getContents();
		final List<IntegerStruct> contents2 = null; //TODO bitArray2.getContents();
		final List<IntegerStruct> contentsToUpdate = null; //TODO bitArrayToUpdate.getContents();

		final int totalSize = arrayTotalSize().toJavaInt();
		for (int i = 0; i < totalSize; i++) {
			final IntegerStruct newBit = bitLogicFunction.apply(new BitLogicContents(contents1, contents2, i));
			contentsToUpdate.set(i, newBit);
		}

		return bitArrayToUpdate;
	}

	// TODO: Make 'private' when we have Java 9
	default BitArrayStruct getBitArrayToUpdate(final LispStruct optArg, final List<Integer> dimensions1) {
		final BitArrayStruct bitArrayToUpdate;
		if (TStruct.INSTANCE.eq(optArg)) {
			bitArrayToUpdate = this;
		} else if (NILStruct.INSTANCE.eq(optArg)) {
			bitArrayToUpdate = copyBitArray();
		} else if (optArg instanceof BitArrayStruct) {
			bitArrayToUpdate = (BitArrayStruct) optArg;
			final List<Integer> optArgDimensions = bitArrayToUpdate.getDimensions();
			// TODO: check this dimension equality
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

	static BitVectorStruct.Builder builder(final IntegerStruct size) {
		return BitVectorStruct.builder(size);
	}

	static BitArrayStruct.Builder builder(final IntegerStruct... dimensions) {
		return new BitArrayStruct.Builder(dimensions);
	}

	final class Builder extends ArrayStruct.AbstractBuilder<BitArrayStruct, BitType, IntegerStruct> {

		private final IntegerStruct[] dimensions;

		private Builder(final IntegerStruct... dimensions) {
			super(BitType.INSTANCE, IntegerStruct.ZERO);
			this.dimensions = dimensions;
		}

		@Override
		public BitArrayStruct.Builder elementType(final BitType elementType) {
			this.elementType = elementType;
			return this;
		}

		@Override
		public BitArrayStruct.Builder initialElement(final IntegerStruct initialElement) {
			this.initialElement = initialElement;
			return this;
		}

		@Override
		public BitArrayStruct.Builder initialContents(final SequenceStruct initialContents) {
			this.initialContents = initialContents;
			return this;
		}

		@Override
		public BitArrayStruct.Builder adjustable(final BooleanStruct adjustable) {
			this.adjustable = adjustable;
			return this;
		}

		@Override
		public BitArrayStruct.Builder fillPointer(final IntegerStruct fillPointer) {
			throw new ErrorException("Non-vector arrays cannot adjust fill-pointer.");
		}

		@Override
		public BitArrayStruct.Builder displacedTo(final ArrayStruct displacedTo) {
			this.displacedTo = displacedTo;
			return this;
		}

		@Override
		public BitArrayStruct.Builder displacedIndexOffset(final IntegerStruct displacedIndexOffset) {
			this.displacedIndexOffset = displacedIndexOffset;
			return this;
		}

		@Override
		public BitArrayStruct build() {
			final LispType upgradedET = ArrayStruct.upgradedArrayElementType(elementType);
			final boolean adjustableBoolean = adjustable.toJavaPBoolean();

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
					return new NILBitArrayStructImpl(ArrayType.INSTANCE,
//					                                 upgradedET,
                                                     displacedTo,
                                                     displacedIndexOffset.toJavaInt(),
                                                     adjustableBoolean);
				}

				final List<Integer> dimensionInts = Arrays.stream(dimensions)
				                                          .map(IntegerStruct::toJavaInt)
				                                          .collect(Collectors.toList());
				return new MultiBitArrayStructImpl(ArrayType.INSTANCE,
				                                   dimensionInts,
//				                                   upgradedET,
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
					return new NILBitArrayStructImpl(arrayType,
//					                                upgradedET,
                                                     initialContents,
                                                     adjustableBoolean);
				}

				final List<Integer> dimensionInts = Arrays.stream(dimensions)
				                                          .map(IntegerStruct::toJavaInt)
				                                          .collect(Collectors.toList());
				final List<LispStruct> validContents = ArrayStruct.getValidContents(dimensionInts,
				                                                                    upgradedET,
				                                                                    initialContents);
				return new MultiBitArrayStructImpl(arrayType,
				                                   dimensionInts,
//				                                  upgradedET,
                                                   validContents,
                                                   adjustableBoolean);
			} else {
				final LispType initialElementType = initialElement.getType();
				if (!upgradedET.typeEquals(initialElementType)) {
					throw new TypeErrorException(
							"Provided element " + initialElement + " is not a subtype of the upgraded-array-element-type " + upgradedET + '.');
				}

				if (dimensions.length == 0) {
					return new NILBitArrayStructImpl(arrayType,
//					                                upgradedET,
                                                     initialElement,
                                                     adjustableBoolean);
				}

				final List<Integer> dimensionInts = Arrays.stream(dimensions)
				                                          .map(IntegerStruct::toJavaInt)
				                                          .collect(Collectors.toList());
				final int totalSize = dimensionInts.stream()
				                                   .mapToInt(Integer::intValue)
				                                   .reduce(1, (x, y) -> x * y);
				final List<LispStruct> contents = Stream.generate(() -> initialElement)
				                                        .limit(totalSize)
				                                        .collect(Collectors.toList());
				return new MultiBitArrayStructImpl(arrayType,
				                                   dimensionInts,
//				                                  upgradedET,
                                                   contents,
                                                   adjustableBoolean);
			}
		}
	}
}
