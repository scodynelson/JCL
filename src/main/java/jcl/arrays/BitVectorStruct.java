package jcl.arrays;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import jcl.conditions.exceptions.TypeErrorException;
import jcl.numbers.IntegerStruct;
import jcl.types.Bit;
import jcl.types.BitVector;
import jcl.types.SimpleBitVector;

/**
 * The {@link BitVectorStruct} is the object representation of a Lisp 'bit-vector' type.
 */
public class BitVectorStruct extends VectorStruct<IntegerStruct> {

	private static final long serialVersionUID = -5044373666080325007L;

	private static final IntegerStruct ZERO = new IntegerStruct(BigInteger.ZERO);
	private static final IntegerStruct ONE = new IntegerStruct(BigInteger.ONE);

	private static final Pattern BIT_PATTERN = Pattern.compile("[0|1]+");

	/**
	 * Public constructor.
	 *
	 * @param bitString
	 * 		a Java string used for the bit-vector contents
	 */
	public BitVectorStruct(final String bitString) {
		this(bitString.length(), getBitList(bitString), false, null);
	}

	/**
	 * Public constructor.
	 *
	 * @param size
	 * 		the bit-vector size
	 * @param contents
	 * 		the bit-vector contents
	 * @param isAdjustable
	 * 		whether or not the bit-vector is adjustable
	 * @param fillPointer
	 * 		the bit-vector fillPointer
	 */
	public BitVectorStruct(final int size, final List<IntegerStruct> contents, final boolean isAdjustable, final Integer fillPointer) {
		super(getBitVectorType(isAdjustable, fillPointer), size, contents, Bit.INSTANCE, isAdjustable, fillPointer);
	}

	/**
	 * Gets the bit-vector type from the provided isAdjustable and fillPointer values.
	 *
	 * @param isAdjustable
	 * 		whether or not the bit-vector is adjustable
	 * @param fillPointer
	 * 		the bit-vector fillPointer
	 *
	 * @return the matching bit-vector type for the provided isAdjustable and fillPointer values
	 */
	private static BitVector getBitVectorType(final boolean isAdjustable, final Integer fillPointer) {
		return (isAdjustable || (fillPointer != null)) ? BitVector.INSTANCE : SimpleBitVector.INSTANCE;
	}

	/**
	 * Gets a list of {@link IntegerStruct}s from the provided {@link String} value.
	 *
	 * @param bitString
	 * 		the Java string to convert to a list of {@link IntegerStruct}s
	 *
	 * @return a list of {@link IntegerStruct}s from the provided {@link String} value
	 */
	private static List<IntegerStruct> getBitList(final String bitString) {
		if (!BIT_PATTERN.matcher(bitString).matches()) {
			throw new TypeErrorException("Input contains characters not of type " + Bit.INSTANCE + ": " + bitString + '.');
		}

		final List<IntegerStruct> bitList = new ArrayList<>(bitString.length());
		for (final char character : bitString.toCharArray()) {
			if (character == '0') {
				bitList.add(ZERO);
			} else if (character == '1') {
				bitList.add(ONE);
			}
		}
		return bitList;
	}
}
