/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;

import org.apache.commons.math3.fraction.BigFraction;

public class BigFractionUtil {

	/**
	 * Constructs a new {@code BigFraction} from the given {@code BigDecimal} object.
	 */
	public static BigFraction getBigFraction(BigDecimal d) {
		//BigDecimal format: unscaled / 10^scale.
		BigInteger tmpNumerator = d.unscaledValue();
		BigInteger tmpDenominator = BigInteger.ONE;

		//Special case for d == 0 (math below won't work right)
		//Note:  Cannot use d.equals(BigDecimal.ZERO), because BigDecimal.equals()
		//       does not consider numbers equal if they have different scales. So,
		//       0.00 is not equal to BigDecimal.ZERO.
		if (tmpNumerator.equals(BigInteger.ZERO)) {
			return BigFraction.ZERO;
		}

		if (d.scale() < 0) {
			tmpNumerator = tmpNumerator.multiply(BigInteger.TEN.pow(-d.scale()));
		} else if (d.scale() > 0) {
			//Now we have the form:  unscaled / 10^scale = unscaled / (2^scale * 5^scale)
			//We know then that gcd(unscaled, 2^scale * 5^scale) = 2^commonTwos * 5^commonFives

			//Easy to determine commonTwos
			int commonTwos = Math.min(d.scale(), tmpNumerator.getLowestSetBit());
			tmpNumerator = tmpNumerator.shiftRight(commonTwos);
			tmpDenominator = tmpDenominator.shiftLeft(d.scale() - commonTwos);

			//Determining commonFives is a little trickier..
			int commonFives = 0;

            /*
             * while(commonFives < d.scale() && tmpNumerator % 5 == 0)
             * { tmpNumerator /= 5; commonFives++; }
             */
			BigInteger[] divMod = null;
			final BigInteger bigIntegerFive = BigInteger.valueOf(5);
			while ((commonFives < d.scale()) && BigInteger.ZERO.equals(
					(divMod = tmpNumerator.divideAndRemainder(bigIntegerFive))[1])) {
				tmpNumerator = divMod[0];
				commonFives++;
			}

			if (commonFives < d.scale()) {
				tmpDenominator = tmpDenominator.multiply(bigIntegerFive.pow(d.scale() - commonFives));
			}
		}
		//else: d.scale() == 0: do nothing

		//Guaranteed there is no gcd, so fraction is in lowest terms
		return new BigFraction(tmpNumerator, tmpDenominator);
	}
}
