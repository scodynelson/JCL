package jcl.structs.arrays;

import jcl.structs.conditions.exceptions.SimpleErrorException;
import jcl.structs.conditions.exceptions.TypeErrorException;
import jcl.structs.numbers.IntegerStruct;
import jcl.types.arrays.BitVector;
import jcl.types.arrays.SimpleBitVector;
import jcl.types.numbers.Bit;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

/**
 * The {@code BitVectorStruct} is the object representation of a Lisp 'bit-vector' type.
 */
public class BitVectorStruct extends VectorStruct<IntegerStruct> {

	/**
	 * Public constructor.
	 *
	 * @param bitString a Java string used for the bit-vector contents
	 * @throws TypeErrorException   if any of the provided {@code contents} are not the same type as the provided {@code elementType}
	 * @throws SimpleErrorException if the provided {@code contents} do not match the provided {@code dimensions}
	 */
	public BitVectorStruct(final String bitString) throws TypeErrorException, SimpleErrorException {
		this(bitString.length(), getBitList(bitString), false, null);
	}

	/**
	 * Public constructor.
	 *
	 * @param size         the bit-vector size
	 * @param contents     the bit-vector contents
	 * @param isAdjustable whether or not the bit-vector is adjustable
	 * @param fillPointer  the bit-vector fillPointer
	 * @throws TypeErrorException   if any of the provided {@code contents} are not the same type as the provided {@code elementType}
	 * @throws SimpleErrorException if the provided {@code contents} do not match the provided {@code dimensions}
	 */
	public BitVectorStruct(final int size, final List<IntegerStruct> contents, final boolean isAdjustable, final Integer fillPointer)
			throws TypeErrorException, SimpleErrorException {
		super(getBitVectorType(isAdjustable, fillPointer), size, contents, Bit.INSTANCE, isAdjustable, fillPointer);
	}

	/**
	 * This method gets the bit-vector type from the provided isAdjustable and fillPointer values.
	 *
	 * @param isAdjustable whether or not the bit-vector is adjustable
	 * @param fillPointer  the bit-vector fillPointer
	 * @return the matching bit-vector type for the provided isAdjustable and fillPointer values
	 */
	private static BitVector getBitVectorType(final boolean isAdjustable, final Integer fillPointer) {
		return (isAdjustable || (fillPointer != null)) ? BitVector.INSTANCE : SimpleBitVector.INSTANCE;
	}

	/**
	 * This method gets a list of IntegerStruct from the provided Java string value.
	 *
	 * @param bitString the Java string to convert to a list of IntegerStruct
	 * @return a list of IntegerStruct from the provided Java string value
	 * @throws TypeErrorException if the bitString contains a character element that is not a 'bit' type
	 */
	private static List<IntegerStruct> getBitList(final String bitString) throws TypeErrorException {
		final IntegerStruct zero = new IntegerStruct(BigInteger.ZERO);
		final IntegerStruct one = new IntegerStruct(BigInteger.ONE);

		final List<IntegerStruct> bitList = new ArrayList<>(bitString.length());
		for (final char character : bitString.toCharArray()) {
			if (character == '0') {
				bitList.add(zero);
			} else if (character == '1') {
				bitList.add(one);
			} else {
				throw new TypeErrorException("Element is not of type " + Bit.INSTANCE + ": " + character + '.');
			}
		}
		return bitList;
	}

	@Override
	public String toString() {
		return "BitVectorStruct{" +
				"contents=" + contents +
				", rank=" + rank +
				", dimensions=" + dimensions +
				", elementType=" + elementType +
				", isAdjustable=" + isAdjustable +
				"fillPointer=" + fillPointer +
				'}';
	}
}
