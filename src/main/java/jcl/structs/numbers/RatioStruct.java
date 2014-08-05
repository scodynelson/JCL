package jcl.structs.numbers;

import jcl.structs.symbols.variables.Variable;
import jcl.types.Ratio;
import org.apache.commons.math3.fraction.BigFraction;

import java.math.BigInteger;

/**
 * The {@link RatioStruct} is the object representation of a Lisp 'ratio' type.
 */
public class RatioStruct extends RationalStruct {

	private final BigFraction bigFraction;

	private static final int SIXTEEN = 16;

	/**
	 * Public constructor.
	 *
	 * @param bigFraction the value of the RatioStruct
	 */
	public RatioStruct(final BigFraction bigFraction) {
		super(Ratio.INSTANCE, null, null);
		this.bigFraction = bigFraction;
	}

	/**
	 * Public constructor.
	 *
	 * @param numerator   the numerator value of the RatioStruct
	 * @param denominator the denominator value of the RatioStruct
	 */
	public RatioStruct(final BigInteger numerator, final BigInteger denominator) {
		super(Ratio.INSTANCE, null, null);
		bigFraction = new BigFraction(numerator, denominator);
	}

	/**
	 * This method returns the value of the RatioStruct.
	 *
	 * @return value of the RatioStruct
	 */
	public BigFraction getBigFraction() {
		return bigFraction;
	}

	@Override
	public String printStruct() {
		return printBigInteger(bigFraction.getNumerator()) + '/' + printBigInteger(bigFraction.getDenominator());
	}

	/**
	 * Private method for getting a String representation of the BigInteger parts of the RatioStruct's internal bigFraction
	 * value.
	 *
	 * @param bigInteger a bigInteger part of the RatioStruct's internal bigFraction value
	 * @return a String representation of the BigInteger parts of the RatioStruct's internal bigFraction value
	 */
	private static String printBigInteger(final BigInteger bigInteger) {
		final boolean printRadix = Variable.PRINT_RADIX.getValue().booleanValue();
		final int printBase = Variable.PRINT_BASE.getValue().getBigInteger().intValue();

		final StringBuilder stringBuilder = new StringBuilder();

		if (printRadix) {
			if (printBase == 2) {
				stringBuilder.append("#b");
			} else if (printBase == 8) {
				stringBuilder.append("#o");
			} else if (printBase == SIXTEEN) {
				stringBuilder.append("#x");
			} else {
				stringBuilder.append('#');
				stringBuilder.append(printBase);
				stringBuilder.append('r');
			}
		}

		stringBuilder.append(bigInteger.toString(printBase));

		return stringBuilder.toString();
	}

	@Override
	public String toString() {
		return "RatioStruct{"
				+ "bigFraction=" + bigFraction
				+ '}';
	}
}
