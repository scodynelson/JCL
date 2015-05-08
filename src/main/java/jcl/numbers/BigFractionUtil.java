/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;

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

	/**
	 * The hypotenuse.
	 *
	 * @param x
	 * 		the first argument.
	 * @param y
	 * 		the second argument.
	 *
	 * @return the square root of the sum of the squares of the two arguments, sqrt(x^2+y^2).
	 */
	static public BigDecimal hypot(final BigDecimal x, final BigDecimal y) {
/* compute x^2+y^2
*/
		BigDecimal z = x.pow(2).add(y.pow(2));
/* truncate to the precision set by x and y. Absolute error = 2*x*xerr+2*y*yerr,
* where the two errors are 1/2 of the ulp’s. Two intermediate protectio digits.
*/
		BigDecimal zerr = x.abs().multiply(x.ulp()).add(y.abs().multiply(y.ulp()));
		MathContext mc = new MathContext(2 + err2prec(z, zerr));
/* Pull square root */
		z = sqrt(z.round(mc));
/* Final rounding. Absolute error in the square root is (y*yerr+x*xerr)/z, where zerr holds 2*(x*xerr+y*yerr).
*/
		mc = new MathContext(err2prec(z.doubleValue(), 0.5 * zerr.doubleValue() / z.doubleValue()));
		return z.round(mc);
	} /* BigDecimalMath.hypot */

	/** The square root.
	 * @param x the non-negative argument.
	 * @return the square root of the BigDecimal rounded to the precision implied by x.
	 */
	static public BigDecimal sqrt(final BigDecimal x) {
		if (x.compareTo(BigDecimal.ZERO) < 0) {
			throw new ArithmeticException("negative argument " + x.toString() + " of square root");
		}
		return root(2, x);
	} /* BigDecimalMath.sqrt */

	/** The integer root.
	 * @param n the positive argument.
	 * @param x the non-negative argument.
	 * @return The n-th root of the BigDecimal rounded to the precision implied by x, x^(1/n).
	 */
	static public BigDecimal root(final int n, final BigDecimal x) {
		if (x.compareTo(BigDecimal.ZERO) < 0) {
			throw new ArithmeticException("negative argument " + x.toString() + " of root");
		}
		if (n <= 0) {
			throw new ArithmeticException("negative power " + n + " of root");
		}
		if (n == 1) {
			return x;
		}
        /* start the computation from a double precision estimate */
		BigDecimal s = new BigDecimal(Math.pow(x.doubleValue(), 1.0 / n));
        /* this creates nth with nominal precision of 1 digit
         */
		final BigDecimal nth = new BigDecimal(n);
        /* Specify an internal accuracy within the loop which is
         * slightly larger than what is demanded by ’eps’ below.
         */
		final BigDecimal xhighpr = scalePrec(x, 2);
		MathContext mc = new MathContext(2 + x.precision());
        /* Relative accuracy of the result is eps.
         */
		final double eps = x.ulp().doubleValue() / (2 * n * x.doubleValue());
		for (;;) {
            /* s = s -(s/n-x/n/s^(n-1)) = s-(s-x/s^(n-1))/n; test correction s/n-x/s for being
             * smaller than the precision requested. The relative correction is (1-x/s^n)/n,
             */
			BigDecimal c = xhighpr.divide(s.pow(n - 1), mc);
			c = s.subtract(c);
			MathContext locmc = new MathContext(c.precision());
			c = c.divide(nth, locmc);
			s = s.subtract(c);
			if (Math.abs(c.doubleValue() / s.doubleValue()) < eps) {
				break;
			}
		}
		return s.round(new MathContext(err2prec(eps)));
	} /* BigDecimalMath.root */

	/** Append decimal zeros to the value. This returns a value which appears to have
	 * a higher precision than the input.
	 * @param x The input value
	 * @param d The (positive) value of zeros to be added as least significant digits.
	 * @return The same value as the input but with increased (pseudo) precision.
	 */
	static public BigDecimal scalePrec(final BigDecimal x, int d) {
		return x.setScale(d + x.scale());


	}
	/** Convert an absolute error to a precision.
	 * @param x The value of the variable
	 * @param xerr The absolute error in the variable
	 * @return The number of valid digits in x.
	 * The value is rounded down, and on the pessimistic side for that reason.
	 */
	static public int err2prec(BigDecimal x, BigDecimal xerr) {
		return err2prec(xerr.divide(x, MathContext.DECIMAL128).doubleValue());
	}

	/** Convert an absolute error to a precision.
	 * @param x The value of the variable
	 * The value returned depends only on the absolute value, not on the sign.
	 * @param xerr The absolute error in the variable
	 * The value returned depends only on the absolute value, not on the sign.
	 * @return The number of valid digits in x.
	 * Derived from the representation x+- xerr, as if the error was represented
	38
	 * in a "half width" (half of the error bar) form.
	 * The value is rounded down, and on the pessimistic side for that reason.
	 */
	static public int err2prec(double x, double xerr) {
        /* Example: an error of xerr=+-0.5 at x=100 represents 100+-0.5 with
         * a precision = 3 (digits).
         */
		return 1 + (int) (Math.log10(Math.abs(0.5 * x / xerr)));
	}
	/** Convert a relative error to a precision.
	 * @param xerr The relative error in the variable.
	 * The value returned depends only on the absolute value, not on the sign.
	 * @return The number of valid digits in x.
	 * The value is rounded down, and on the pessimistic side for that reason.
	 */
	static public int err2prec(double xerr) {
        /* Example: an error of xerr=+-0.5 a precision of 1 (digit), an error of
         * +-0.05 a precision of 2 (digits)
         */
		return 1 + (int) (Math.log10(Math.abs(0.5 / xerr)));
	}
}
