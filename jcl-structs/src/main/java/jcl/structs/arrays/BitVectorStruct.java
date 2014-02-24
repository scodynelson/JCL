package jcl.structs.arrays;

import jcl.structs.numbers.IntegerStruct;
import jcl.types.LispType;
import jcl.types.arrays.BitVector;
import jcl.types.numbers.Bit;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

public class BitVectorStruct extends VectorStruct<IntegerStruct> {
	// TODO: this should be a "bit" type integer structure

	public BitVectorStruct(final String bitString) {
		this(bitString.length(), getBitList(bitString), Bit.INSTANCE, false, null);
	}

	public BitVectorStruct(final int size, final List<IntegerStruct> contents, final LispType elementType,
						   final boolean isAdjustable, final Integer fillPointer) {
		super(BitVector.INSTANCE, size, contents, elementType, isAdjustable, fillPointer);
	}

	private static List<IntegerStruct> getBitList(final String bitString) {
		final IntegerStruct zero = new IntegerStruct(BigInteger.ZERO);
		final IntegerStruct one = new IntegerStruct(BigInteger.ONE);

		final List<IntegerStruct> bitList = new ArrayList<>(bitString.length());
		for (final char character : bitString.toCharArray()) {
			if (character == '0') {
				bitList.add(zero);
			} else if (character == '1') {
				bitList.add(one);
			}
			// TODO: handle exceptions...
		}
		return bitList;
	}
}
