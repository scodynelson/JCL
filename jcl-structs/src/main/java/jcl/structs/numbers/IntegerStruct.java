package jcl.structs.numbers;

import jcl.types.numbers.Integer;

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
		super(Integer.INSTANCE, null, null); // TODO: determine the type???
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
		return "IntegerStruct{" +
				"bigInteger=" + bigInteger +
				'}';
	}
}
