package jcl.structs.numbers;

import jcl.types.LispType;
import jcl.types.numbers.Integer;

import java.math.BigInteger;

/**
 * The {@code IntegerStruct} is the object representation of a Lisp 'integer' type.
 */
public class IntegerStruct extends RationalStruct {

	private final BigInteger bigInteger;

	/**
	 * Private constructor.
	 *
	 * @param bigInteger the value of the {@code IntegerStruct}
	 */
	private IntegerStruct(final BigInteger bigInteger) {
		this.bigInteger = bigInteger;
	}

	/**
	 * This method returns the value of the {@code IntegerStruct}.
	 *
	 * @return value of the {@code IntegerStruct}
	 */
	public BigInteger getBigInteger() {
		return bigInteger;
	}

	@Override
	public LispType getType() {
		return Integer.INSTANCE;
	}

	@Override
	public String toString() {
		return "IntegerStruct{" +
				"bigInteger=" + bigInteger +
				'}';
	}

	// BUILDERS

	/**
	 * This method gets the {@code IntegerStruct} for the provided {@code bigInteger}.
	 *
	 * @param bigInteger the value of the {@code IntegerStruct}
	 * @return the created {@code IntegerStruct}
	 */
	public static IntegerStruct getStruct(final BigInteger bigInteger) {
		return new IntegerStruct(bigInteger);
	}
}
