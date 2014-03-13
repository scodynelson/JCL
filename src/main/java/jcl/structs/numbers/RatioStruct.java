package jcl.structs.numbers;

import jcl.types.Ratio;
import org.apache.commons.math3.fraction.BigFraction;

import java.math.BigInteger;

/**
 * The {@code RatioStruct} is the object representation of a Lisp 'ratio' type.
 */
public class RatioStruct extends RationalStruct {

	private final BigFraction bigFraction;

	/**
	 * Public constructor.
	 *
	 * @param bigFraction the value of the {@code RatioStruct}
	 */
	public RatioStruct(final BigFraction bigFraction) {
		super(Ratio.INSTANCE, null, null);
		this.bigFraction = bigFraction;
	}

	/**
	 * Public constructor.
	 *
	 * @param numerator   the numerator value of the {@code RatioStruct}
	 * @param denominator the denominator value of the {@code RatioStruct}
	 */
	public RatioStruct(final BigInteger numerator, final BigInteger denominator) {
		super(Ratio.INSTANCE, null, null);
		bigFraction = new BigFraction(numerator, denominator);
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
		return "RatioStruct{"
				+ "bigFraction=" + bigFraction
				+ '}';
	}

}
