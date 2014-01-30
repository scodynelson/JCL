package jcl.structs.numbers;

import org.apache.commons.math3.fraction.BigFraction;

import java.math.BigInteger;

/**
 * The {@code RatioStruct} is the object representation of a Lisp 'ratio' type.
 */
public class RatioStruct extends RationalStruct {

	private final BigFraction bigFraction;

	/**
	 * Private constructor.
	 *
	 * @param bigFraction the value of the {@code RatioStruct}
	 */
	private RatioStruct(final BigFraction bigFraction) {
		this.bigFraction = bigFraction;
	}

	/**
	 * This method returns the value of the {@code RatioStruct}.
	 *
	 * @return value of the {@code RatioStruct}
	 */
	public BigFraction getBigFraction() {
		return bigFraction;
	}

	@Override
	public String toString() {
		return "RatioStruct{" +
				"bigFraction=" + bigFraction +
				'}';
	}

	// BUILDERS

	/**
	 * This method gets the {@code RatioStruct} for the provided {@code bigFraction}.
	 *
	 * @param bigFraction the value of the {@code RatioStruct}
	 * @return the created {@code RatioStruct}
	 */
	public static RatioStruct getStruct(final BigFraction bigFraction) {
		return new RatioStruct(bigFraction);
	}

	/**
	 * This method gets the {@code RatioStruct} for the provided {@code numerator} and {@code denominator}.
	 *
	 * @param numerator   the numerator value of the {@code RatioStruct}
	 * @param denominator the denominator value of the {@code RatioStruct}
	 * @return the created {@code RatioStruct}
	 */
	public static RatioStruct getStruct(final BigInteger numerator, final BigInteger denominator) {
		final BigFraction bigFraction = new BigFraction(numerator, denominator);
		return new RatioStruct(bigFraction);
	}
}
