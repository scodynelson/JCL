package jcl.structs.arrays;

import jcl.structs.numbers.IntegerStruct;
import jcl.types.LispType;
import jcl.types.numbers.Bit;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

public class BitVectorStruct extends VectorStruct<IntegerStruct> {
	// TODO: this should be a "bit" type integer structure

	protected BitVectorStruct(final int size, final List<IntegerStruct> contents, final LispType elementType,
							  final boolean isAdjustable, final Integer fillPointer) {
		super(size, contents, elementType, isAdjustable, fillPointer);
	}

	// BUILDERS

	public static BitVectorStruct getStruct(final String bitString) {

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
		return new BitVectorStruct(bitList.size(), bitList, Bit.INSTANCE, false, null);
	}
}
