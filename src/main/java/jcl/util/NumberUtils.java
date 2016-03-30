package jcl.util;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.RoundingMode;

public class NumberUtils {

	public static BigDecimal bigDecimalValue(final int i) {
		return BigDecimal.valueOf(i).setScale(1, RoundingMode.HALF_EVEN);
	}

	public static BigDecimal bigDecimalValue(final long l) {
		return BigDecimal.valueOf(l).setScale(1, RoundingMode.HALF_EVEN);
	}

	public static BigDecimal bigDecimalValue(final BigInteger bigInteger) {
		return new BigDecimal(bigInteger).setScale(1, RoundingMode.HALF_EVEN);
	}

	public static BigDecimal bigDecimalValue(final float f) {
		// NOTE: Using 'String.valueOf' since the BigDecimal#valueOf(double) does some floating point rounding crap that
		//          changes the actual value since it stores it as a 'long' under the hood.
		return new BigDecimal(String.valueOf(f));
	}

	public static BigDecimal bigDecimalValue(final double d) {
		// NOTE: Using 'String.valueOf' since the BigDecimal#valueOf(double) does some floating point rounding crap that
		//          changes the actual value since it stores it as a 'long' under the hood.
		return new BigDecimal(String.valueOf(d));
	}

	public static BigDecimal bigDecimalValue(final String s) {
		// NOTE: Using 'String.valueOf' since the BigDecimal#valueOf(double) does some floating point rounding crap that
		//          changes the actual value since it stores it as a 'long' under the hood.
		BigDecimal bigDecimal = new BigDecimal(s);
		final int scale = bigDecimal.scale();
		if (scale == 0) {
			bigDecimal = bigDecimal.setScale(1, RoundingMode.HALF_EVEN);
		}
		return bigDecimal;
	}

	public static boolean longFitsInInt(final long l) {
		return (int) l == l;
	}

	public static int longToInt(final long l) {
		return (int) l;
	}

	public static float doubleToFloat(final double d) {
		return (float) d;
	}
}
