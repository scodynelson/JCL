package jcl.lang;

import java.util.List;
import java.util.function.Function;

import jcl.lang.condition.exception.ErrorException;
import jcl.lang.condition.exception.TypeErrorException;
import jcl.type.LispType;
import jcl.type.SimpleArrayType;

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
}
