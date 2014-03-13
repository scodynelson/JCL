package jcl.structs;

import jcl.structs.conditions.exceptions.TypeErrorException;
import jcl.types.BitVector;
import jcl.types.SimpleBitVector;
import jcl.types.Bit;

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
	 */
	public BitVectorStruct(final String bitString) {
		this(bitString.length(), getBitList(bitString), false, null);
	}

	/**
	 * Public constructor.
	 *
	 * @param size         the bit-vector size
	 * @param contents     the bit-vector contents
	 * @param isAdjustable whether or not the bit-vector is adjustable
	 * @param fillPointer  the bit-vector fillPointer
	 */
	public BitVectorStruct(final int size, final List<IntegerStruct> contents, final boolean isAdjustable, final Integer fillPointer) {
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
	 */
	private static List<IntegerStruct> getBitList(final String bitString) {
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
		return "BitVectorStruct{"
				+ "contents=" + contents
				+ ", rank=" + rank
				+ ", dimensions=" + dimensions
				+ ", elementType=" + elementType
				+ ", isAdjustable=" + isAdjustable
				+ "fillPointer=" + fillPointer
				+ '}';
	}
}
