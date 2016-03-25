package jcl.util;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

public class NumberUtils {

	public static BigDecimal bigDecimalValue(final BigInteger bigInteger) {
		return new BigDecimal(bigInteger, 1).multiply(BigDecimal.TEN);
	}

	public static BigInteger sqrt(final BigInteger bigInteger) {
		BigInteger a = BigInteger.ONE;
		BigInteger b = new BigInteger(bigInteger.shiftRight(5).add(new BigInteger("8")).toString());
		while (b.compareTo(a) >= 0) {
			BigInteger mid = new BigInteger(a.add(b).shiftRight(1).toString());
			if (mid.multiply(mid).compareTo(bigInteger) > 0) {
				b = mid.subtract(BigInteger.ONE);
			} else {
				a = mid.add(BigInteger.ONE);
			}
		}
		return a.subtract(BigInteger.ONE);
	}

	//--------------------------------


	protected static BigInteger BigIntegerZERO = BigInteger.ZERO;
	protected static BigInteger BigIntegerONE = BigInteger.ONE;
	protected static BigInteger BigIntegerTWO = BigInteger.valueOf(2);
	protected static BigDecimal BigDecimalZERO = BigDecimal.ZERO;
	protected static BigDecimal BigDecimalONE = BigDecimal.ONE;
	protected static BigDecimal BigDecimalTWO = new BigDecimal(2);

	public static BigInteger sqrt2(BigInteger number) {
		return sqrt(number, BigIntegerONE);
	}

	public static BigDecimal sqrt(BigDecimal number, RoundingMode rounding) {
		return sqrt(number, BigDecimalONE, rounding);
	}

	protected static BigInteger sqrt(BigInteger number, BigInteger guess) {
		// redoing this to avoid StackOverFlow
		BigInteger result = BigIntegerZERO;
		BigInteger flipA = result;
		BigInteger flipB = result;
		boolean first = true;
		while (result.compareTo(guess) != 0) {
			if (!first)
				guess = result;
			else
				first = false;

			result = number.divide(guess).add(guess).divide(BigIntegerTWO);
			// handle flip flops
			if (result.equals(flipB))
				return flipA;

			flipB = flipA;
			flipA = result;
		}
		return result;

	}

	public static BigDecimal sqrt(BigDecimal number, BigDecimal guess, RoundingMode rounding) {
		BigDecimal result = BigDecimalZERO;
		BigDecimal flipA = result;
		BigDecimal flipB = result;
		boolean first = true;
		while (result.compareTo(guess) != 0) {
			if (!first)
				guess = result;
			else
				first = false;

			result = number.divide(guess, rounding).add(guess).divide(BigDecimalTWO, rounding);
			// handle flip flops
			if (result.equals(flipB))
				return flipA;

			flipB = flipA;
			flipA = result;
		}
		return result;
	}


	public static BigInteger sqrt3(BigInteger x) {
		BigInteger div = BigInteger.ZERO.setBit(x.bitLength()/2);
		BigInteger div2 = div;
		// Loop until we hit the same value twice in a row, or wind
		// up alternating.
		for(;;) {
			BigInteger y = div.add(x.divide(div)).shiftRight(1);
			if (y.equals(div) || y.equals(div2))
				return y;
			div2 = div;
			div = y;
		}
	}


}
