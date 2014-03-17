package jcl.numbers;

import jcl.types.Integer;

import java.math.BigInteger;

/**
 * The {@code IntegerStruct} is the object representation of a Lisp 'integer' type.
 */
public class IntegerStruct extends RationalStruct {

	private final BigInteger bigInteger;

	/**
	 * Public constructor.
	 *
	 * @param bigInteger the value of the {@code IntegerStruct}
	 */
	public IntegerStruct(final BigInteger bigInteger) {
		this(Integer.INSTANCE, bigInteger);
	}

	/**
	 * Public constructor.
	 *
	 * @param integerFormat a {@code Integer} that represents the type of {@code Integer}
	 * @param bigInteger    the value of the {@code IntegerStruct}
	 */
	public IntegerStruct(final Integer integerFormat, final BigInteger bigInteger) {
		super(integerFormat, null, null);
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
	public String toString() {
		return "IntegerStruct{"
				+ "bigInteger=" + bigInteger
				+ '}';
	}
}
