package jcl.util;

import java.math.BigDecimal;
import java.math.RoundingMode;

import lombok.experimental.UtilityClass;

@UtilityClass
public final class NumberUtils {

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
}
