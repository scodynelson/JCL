package jcl.arrays;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import jcl.conditions.exceptions.TypeErrorException;
import jcl.numbers.IntegerStruct;
import jcl.types.BitType;
import jcl.types.BitVectorType;
import jcl.types.SimpleBitVectorType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * The {@link BitVectorStruct} is the object representation of a Lisp 'bit-vector' type.
 */
public class BitVectorStruct extends VectorStruct<IntegerStruct> {

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
	 * @param contents
	 * 		the bit-vector contents
	 */
	public BitVectorStruct(final List<IntegerStruct> contents) {
		this(contents.size(), contents, false, null);
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
		super(getBitVectorType(isAdjustable, fillPointer), size, contents, BitType.INSTANCE, isAdjustable, fillPointer);
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
	private static BitVectorType getBitVectorType(final boolean isAdjustable, final Integer fillPointer) {
		return (isAdjustable || (fillPointer != null)) ? BitVectorType.INSTANCE : SimpleBitVectorType.INSTANCE;
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
		if (!bitString.isEmpty() && !BIT_PATTERN.matcher(bitString).matches()) {
			throw new TypeErrorException("Input contains characters not of type " + BitType.INSTANCE + ": " + bitString + '.');
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

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .isEquals();
	}
}
