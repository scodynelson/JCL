package jcl.util;

import java.math.BigDecimal;
import java.math.BigInteger;

public class NumberUtils {

	public static BigDecimal bigDecimalValue(final BigInteger bigInteger) {
		return new BigDecimal(bigInteger, 1).multiply(BigDecimal.TEN);
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
