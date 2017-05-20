package jcl.lang.internal;

import java.math.BigInteger;
import java.math.RoundingMode;
import java.util.function.Function;

import com.google.common.math.DoubleMath;
import jcl.lang.FloatStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.NumberStruct;
import jcl.lang.RatioStruct;
import jcl.lang.RationalStruct;
import jcl.lang.RealStruct;
import jcl.lang.SingleFloatStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.number.QuotientRemainder;
import jcl.type.RatioType;
import lombok.EqualsAndHashCode;
import org.apache.commons.math3.fraction.BigFraction;
import org.apfloat.Apcomplex;
import org.apfloat.ApcomplexMath;
import org.apfloat.Apfloat;
import org.apfloat.ApfloatMath;
import org.apfloat.Apint;
import org.apfloat.Aprational;

/**
 * The {@link RatioStruct} is the object representation of a Lisp 'ratio' type.
 */
@EqualsAndHashCode(callSuper = false)
public class RatioStructImpl extends BuiltInClassStruct implements RatioStruct {

	/**
	 * The numerator of the ratio.
	 */
	private final IntegerStruct numerator;

	/**
	 * The denominator of the ratio.
	 */
	private final IntegerStruct denominator;

	/**
	 * The {@link BigFraction} representation of the ratio.
	 */
	final BigFraction value;

	/**
	 * Public constructor.
	 *
	 * @param numerator
	 * 		the {@link IntegerStruct} value of the numerator of the resulting RationalStruct
	 * @param denominator
	 * 		the {@link IntegerStruct} value of the denominator of the resulting RationalStruct
	 */
	public RatioStructImpl(final IntegerStruct numerator, final IntegerStruct denominator) {
		super(RatioType.INSTANCE, null, null);
		this.numerator = numerator;
		this.denominator = denominator;
		value = new BigFraction(numerator.toJavaBigInteger(), denominator.toJavaBigInteger());
	}

	/**
	 * Public constructor.
	 *
	 * @param value
	 * 		the {@link BigFraction} value representing data for the resulting RationalStruct
	 */
	public RatioStructImpl(final BigFraction value) {
		super(RatioType.INSTANCE, null, null);
		numerator = IntegerStruct.toLispInteger(value.getNumerator());
		denominator = IntegerStruct.toLispInteger(value.getDenominator());
		this.value = value;
	}

	@Override
	public BigFraction toJavaBigFraction() {
		return value;
	}

	/*
	RATIONAL-STRUCT
	 */

	@Override
	public IntegerStruct numerator() {
		return numerator;
	}

	@Override
	public IntegerStruct denominator() {
		return denominator;
	}

	/*
	REAL-STRUCT
	 */

	@Override
	public boolean isLessThan(final RealStruct real) {
		if (real instanceof IntegerStruct) {
			final BigFraction numberBF = ((IntegerStruct) real).toJavaBigFraction();
			return value.compareTo(numberBF) < 0;
		} else if (real instanceof RatioStructImpl) {
			return value.compareTo(((RatioStructImpl) real).value) < 0;
		}
		return isLessThan(real.rational());
	}

	@Override
	public boolean isGreaterThan(final RealStruct real) {
		if (real instanceof IntegerStruct) {
			final BigFraction numberBF = ((IntegerStruct) real).toJavaBigFraction();
			return value.compareTo(numberBF) > 0;
		} else if (real instanceof RatioStructImpl) {
			return value.compareTo(((RatioStructImpl) real).value) > 0;
		}
		return isGreaterThan(real.rational());
	}

	@Override
	public boolean isLessThanOrEqualTo(final RealStruct real) {
		if (real instanceof IntegerStruct) {
			final BigFraction numberBF = ((IntegerStruct) real).toJavaBigFraction();
			return value.compareTo(numberBF) <= 0;
		} else if (real instanceof RatioStructImpl) {
			return value.compareTo(((RatioStructImpl) real).value) <= 0;
		}
		return isLessThanOrEqualTo(real.rational());
	}

	@Override
	public boolean isGreaterThanOrEqualTo(final RealStruct real) {
		if (real instanceof IntegerStruct) {
			final BigFraction numberBF = ((IntegerStruct) real).toJavaBigFraction();
			return value.compareTo(numberBF) >= 0;
		} else if (real instanceof RatioStructImpl) {
			return value.compareTo(((RatioStructImpl) real).value) >= 0;
		}
		return isGreaterThanOrEqualTo(real.rational());
	}

	@Override
	public boolean plusp() {
		return BigFraction.ZERO.compareTo(value) > 0;
	}

	@Override
	public boolean minusp() {
		return BigFraction.ZERO.compareTo(value) < 0;
	}

	@Override
	public FloatStruct floatingPoint() {
		// TODO: Default to double-float???
		return new DoubleFloatStructImpl(value.doubleValue());
	}

	@Override
	public FloatStruct floatingPoint(final FloatStruct prototype) {
		if (prototype instanceof SingleFloatStruct) {
			return new SingleFloatStructImpl(value.floatValue());
		} else {
			return new DoubleFloatStructImpl(value.doubleValue());
		}
	}

	private QuotientRemainder calculateQuotientRemainder(final Function<Double, Long> operation,
	                                                     final Function<Long, RealStruct> quotientCreator) {
		final long quotient = operation.apply(value.doubleValue());
		final double remainder = value.doubleValue() - quotient;

		return new QuotientRemainder(
				quotientCreator.apply(quotient),
				new SingleFloatStructImpl(remainder)
		);
	}

	/**
	 * Calculates the quotient remainder using the provided {@code operation} dividing {@link #value} by the provided
	 * {@code divisor}. The resulting quotient will be created using the provided {@code quotientCreator}.
	 *
	 * @param divisor
	 * 		what to divide {@link #value} by before performing the operation
	 * @param operation
	 * 		the quotient/remainder operation to perform
	 * @param quotientCreator
	 * 		the quotient generator
	 *
	 * @return a {@link QuotientRemainder} containing the results of the operation
	 */
	private QuotientRemainder calculateQuotientRemainder(final RealStruct divisor,
	                                                     final Function<Double, Long> operation,
	                                                     final Function<Long, RealStruct> quotientCreator) {

		if (divisor instanceof IntegerStruct) {
			final BigInteger valN = value.getNumerator();
			final BigInteger valD = value.getDenominator();

			final BigInteger divisorValue = ((IntegerStruct) divisor).toJavaBigInteger();

			// Invert and multiply.
			final BigInteger den = valD.multiply(divisorValue);
			final BigInteger quotient = valN.divide(den);

			// Multiply quotient by divisor.
			final BigInteger product = quotient.multiply(divisorValue);
			// Subtract to get remainder.
			final BigFraction remainder = value.subtract(product);

			return new QuotientRemainder(
					IntegerStruct.toLispInteger(quotient),
					RationalStruct.toLispRational(remainder)
			);
		} else if (divisor instanceof RatioStructImpl) {
			final BigInteger valN = value.getNumerator();
			final BigInteger valD = value.getDenominator();

			final BigFraction divisorValue = ((RatioStructImpl) divisor).value;
			final BigInteger divisorN = divisorValue.getNumerator();
			final BigInteger divisorD = divisorValue.getDenominator();

			// Invert and multiply.
			final BigInteger num = valN.multiply(divisorD);
			final BigInteger den = valD.multiply(divisorN);
			final BigInteger quotient = num.divide(den);

			// Multiply quotient by divisor.
			final BigFraction product = new BigFraction(quotient.multiply(divisorN), divisorD);
			// Subtract to get remainder.
			final BigFraction remainder = value.subtract(product);

			return new QuotientRemainder(
					IntegerStruct.toLispInteger(quotient),
					RationalStruct.toLispRational(remainder)
			);
		} else if (divisor instanceof SingleFloatStructImpl) {
			final float val = value.floatValue();
			final float divisorValue = ((SingleFloatStructImpl) divisor).value;
			final double divide = val / divisorValue;

			final long quotient = operation.apply(divide);
			final double remainder = val - (quotient * divisorValue);

			return new QuotientRemainder(
					quotientCreator.apply(quotient),
					new SingleFloatStructImpl(remainder)
			);
		} else if (divisor instanceof DoubleFloatStructImpl) {
			final double val = value.doubleValue();
			final double divisorValue = ((DoubleFloatStructImpl) divisor).value;
			final double divide = val / divisorValue;

			final long quotient = operation.apply(divide);
			final double remainder = val - (quotient * divisorValue);

			return new QuotientRemainder(
					quotientCreator.apply(quotient),
					new DoubleFloatStructImpl(remainder)
			);
		} else {
			final double val = value.doubleValue();
			final double divisorValue = divisor.ap().doubleValue();
			final double divide = val / divisorValue;

			final long quotient = operation.apply(divide);
			final double remainder = val - (quotient * divisorValue);

			return new QuotientRemainder(
					quotientCreator.apply(quotient),
					new SingleFloatStructImpl(remainder)
			);
		}
	}

	/**
	 * Creates a function for creating a new lisp float instance when provided a {@code long} value. If the provided
	 * {@code real} is a single-float, a single-float will be generated; otherwise a double-float will be generated.
	 *
	 * @param real
	 * 		the real used to determine the generating function
	 *
	 * @return a lisp float generating function
	 */
	private static Function<Long, RealStruct> toLispFloat(final RealStruct real) {
		return (real instanceof SingleFloatStructImpl)
		       ? SingleFloatStructImpl::new
		       : DoubleFloatStructImpl::new;
	}

	@SuppressWarnings("NumericCastThatLosesPrecision")
	@Override
	public QuotientRemainder floor() {
		return calculateQuotientRemainder(val -> (long) StrictMath.floor(val),
		                                  IntegerStruct::toLispInteger);
	}

	@SuppressWarnings("NumericCastThatLosesPrecision")
	@Override
	public QuotientRemainder floor(final RealStruct divisor) {
		return calculateQuotientRemainder(divisor, val -> (long) StrictMath.floor(val),
		                                  IntegerStruct::toLispInteger);
	}

	@SuppressWarnings("NumericCastThatLosesPrecision")
	@Override
	public QuotientRemainder ffloor() {
		return calculateQuotientRemainder(val -> (long) StrictMath.floor(val),
		                                  SingleFloatStruct::toLispFloat);
	}

	@SuppressWarnings("NumericCastThatLosesPrecision")
	@Override
	public QuotientRemainder ffloor(final RealStruct divisor) {
		return calculateQuotientRemainder(divisor, val -> (long) StrictMath.floor(val),
		                                  toLispFloat(divisor));
	}

	@SuppressWarnings("NumericCastThatLosesPrecision")
	@Override
	public QuotientRemainder ceiling() {
		return calculateQuotientRemainder(val -> (long) StrictMath.ceil(val),
		                                  IntegerStruct::toLispInteger);
	}

	@SuppressWarnings("NumericCastThatLosesPrecision")
	@Override
	public QuotientRemainder ceiling(final RealStruct divisor) {
		return calculateQuotientRemainder(divisor, val -> (long) StrictMath.ceil(val),
		                                  IntegerStruct::toLispInteger);
	}

	@SuppressWarnings("NumericCastThatLosesPrecision")
	@Override
	public QuotientRemainder fceiling() {
		return calculateQuotientRemainder(val -> (long) StrictMath.ceil(val),
		                                  SingleFloatStruct::toLispFloat);
	}

	@SuppressWarnings("NumericCastThatLosesPrecision")
	@Override
	public QuotientRemainder fceiling(final RealStruct divisor) {
		return calculateQuotientRemainder(divisor, val -> (long) StrictMath.ceil(val),
		                                  toLispFloat(divisor));
	}

	@Override
	public QuotientRemainder truncate() {
		return calculateQuotientRemainder(val -> DoubleMath.roundToLong(value.doubleValue(), RoundingMode.DOWN),
		                                  IntegerStruct::toLispInteger);
	}

	@Override
	public QuotientRemainder truncate(final RealStruct divisor) {
		return calculateQuotientRemainder(divisor,
		                                  val -> DoubleMath.roundToLong(value.doubleValue(), RoundingMode.DOWN),
		                                  IntegerStruct::toLispInteger);
	}

	@Override
	public QuotientRemainder ftruncate() {
		return calculateQuotientRemainder(val -> DoubleMath.roundToLong(value.doubleValue(), RoundingMode.DOWN),
		                                  SingleFloatStruct::toLispFloat);
	}

	@Override
	public QuotientRemainder ftruncate(final RealStruct divisor) {
		return calculateQuotientRemainder(divisor,
		                                  val -> DoubleMath.roundToLong(value.doubleValue(), RoundingMode.DOWN),
		                                  toLispFloat(divisor));
	}

	@Override
	public QuotientRemainder round() {
		return calculateQuotientRemainder(val -> DoubleMath.roundToLong(value.doubleValue(), RoundingMode.HALF_UP),
		                                  IntegerStruct::toLispInteger);
	}

	@Override
	public QuotientRemainder round(final RealStruct divisor) {
		return calculateQuotientRemainder(divisor,
		                                  val -> DoubleMath.roundToLong(value.doubleValue(), RoundingMode.HALF_UP),
		                                  IntegerStruct::toLispInteger);
	}

	@Override
	public QuotientRemainder fround() {
		return calculateQuotientRemainder(val -> DoubleMath.roundToLong(value.doubleValue(), RoundingMode.HALF_UP),
		                                  SingleFloatStruct::toLispFloat);
	}

	@Override
	public QuotientRemainder fround(final RealStruct divisor) {
		return calculateQuotientRemainder(divisor,
		                                  val -> DoubleMath.roundToLong(value.doubleValue(), RoundingMode.HALF_UP),
		                                  toLispFloat(divisor));
	}

	@Override
	public RealStruct atan(final RealStruct real) {
		final Aprational ap = ap();
		final Apfloat realAp = real.ap();
		final Apfloat atan2 = ApfloatMath.atan2(ap, realAp);
		return ApfloatUtils.toRealStruct(atan2);
	}

	/*
	NUMBER-STRUCT
	 */

	@Override
	public Aprational ap() {
		return new Aprational(new Apint(value.getNumerator()), new Apint(value.getDenominator()));
	}

	@Override
	public RatioStruct abs() {
		final BigFraction abs = value.abs();
		return new RatioStructImpl(abs);
	}

	@Override
	public boolean zerop() {
		return BigFraction.ZERO.compareTo(value) == 0;
	}

	@Override
	public NumberStruct add(final NumberStruct number) {
		if (number instanceof FixnumStructImpl) {
			final BigFraction add = value.add(((FixnumStructImpl) number).value);
			return new RatioStructImpl(add);
		} else if (number instanceof LongnumStructImpl) {
			final BigFraction add = value.add(((LongnumStructImpl) number).value);
			return new RatioStructImpl(add);
		} else if (number instanceof BignumStructImpl) {
			final BigFraction add = value.add(((BignumStructImpl) number).value);
			return new RatioStructImpl(add);
		} else if (number instanceof RatioStructImpl) {
			final BigFraction add = value.add(((RatioStructImpl) number).value);
			return RationalStruct.toLispRational(add);
		}
		return number.add(this);
	}

	@Override
	public NumberStruct subtract(final NumberStruct number) {
		if (number instanceof FixnumStructImpl) {
			final BigFraction subtract = value.subtract(((FixnumStructImpl) number).value);
			return new RatioStructImpl(subtract);
		} else if (number instanceof LongnumStructImpl) {
			final BigFraction subtract = value.subtract(((LongnumStructImpl) number).value);
			return new RatioStructImpl(subtract);
		} else if (number instanceof BignumStructImpl) {
			final BigFraction subtract = value.subtract(((BignumStructImpl) number).value);
			return new RatioStructImpl(subtract);
		} else if (number instanceof RatioStructImpl) {
			final BigFraction subtract = value.subtract(((RatioStructImpl) number).value);
			return RationalStruct.toLispRational(subtract);
		} else if (number instanceof SingleFloatStructImpl) {
			final float f = value.floatValue();
			final float subtract = f - ((SingleFloatStructImpl) number).value;
			return new SingleFloatStructImpl(subtract);
		} else if (number instanceof DoubleFloatStructImpl) {
			final double d = value.doubleValue();
			final double subtract = d - ((DoubleFloatStructImpl) number).value;
			return new DoubleFloatStructImpl(subtract);
		}
		final Aprational ap = ap();
		final Apcomplex numberAp = number.ap();
		final Apcomplex subtract = ap.subtract(numberAp);
		return ApfloatUtils.toNumberStruct(subtract);
	}

	@Override
	public NumberStruct multiply(final NumberStruct number) {
		if (number instanceof FixnumStructImpl) {
			final BigFraction multiply = value.multiply(((FixnumStructImpl) number).value);
			return RationalStruct.toLispRational(multiply);
		} else if (number instanceof LongnumStructImpl) {
			final BigFraction multiply = value.multiply(((LongnumStructImpl) number).value);
			return RationalStruct.toLispRational(multiply);
		} else if (number instanceof BignumStructImpl) {
			final BigFraction multiply = value.multiply(((BignumStructImpl) number).value);
			return RationalStruct.toLispRational(multiply);
		} else if (number instanceof RatioStructImpl) {
			final BigFraction multiply = value.multiply(((RatioStructImpl) number).value);
			return RationalStruct.toLispRational(multiply);
		}
		return number.multiply(this);
	}

	@Override
	public NumberStruct divide(final NumberStruct number) {
		if (number instanceof FixnumStructImpl) {
			final BigFraction divide = value.divide(((FixnumStructImpl) number).value);
			return RationalStruct.toLispRational(divide);
		} else if (number instanceof LongnumStructImpl) {
			final BigFraction divide = value.divide(((LongnumStructImpl) number).value);
			return RationalStruct.toLispRational(divide);
		} else if (number instanceof BignumStructImpl) {
			final BigFraction divide = value.divide(((BignumStructImpl) number).value);
			return RationalStruct.toLispRational(divide);
		} else if (number instanceof RatioStructImpl) {
			final BigFraction divide = value.divide(((RatioStructImpl) number).value);
			return RationalStruct.toLispRational(divide);
		} else if (number instanceof SingleFloatStructImpl) {
			final float f = value.floatValue();
			final float divide = f / ((SingleFloatStructImpl) number).value;
			return new SingleFloatStructImpl(divide);
		} else if (number instanceof DoubleFloatStructImpl) {
			final double d = value.doubleValue();
			final double divide = d / ((DoubleFloatStructImpl) number).value;
			return new DoubleFloatStructImpl(divide);
		}
		final Aprational ap = ap();
		final Apcomplex numberAp = number.ap();
		final Apcomplex divide = ap.divide(numberAp);
		return ApfloatUtils.toNumberStruct(divide);
	}

	@Override
	public boolean isEqualTo(final NumberStruct number) {
		if (number instanceof FixnumStructImpl) {
			final BigFraction numberBF = new BigFraction(((FixnumStructImpl) number).value);
			return value.compareTo(numberBF) == 0;
		} else if (number instanceof LongnumStructImpl) {
			final BigFraction numberBF = new BigFraction(((LongnumStructImpl) number).value);
			return value.compareTo(numberBF) == 0;
		} else if (number instanceof BignumStructImpl) {
			final BigFraction numberBF = new BigFraction(((BignumStructImpl) number).value);
			return value.compareTo(numberBF) == 0;
		} else if (number instanceof RatioStructImpl) {
			return value.compareTo(((RatioStructImpl) number).value) == 0;
		}
		return number.isNotEqualTo(this);
	}

	@Override
	public boolean isNotEqualTo(final NumberStruct number) {
		if (number instanceof FixnumStructImpl) {
			final BigFraction numberBF = new BigFraction(((FixnumStructImpl) number).value);
			return value.compareTo(numberBF) != 0;
		} else if (number instanceof LongnumStructImpl) {
			final BigFraction numberBF = new BigFraction(((LongnumStructImpl) number).value);
			return value.compareTo(numberBF) != 0;
		} else if (number instanceof BignumStructImpl) {
			final BigFraction numberBF = new BigFraction(((BignumStructImpl) number).value);
			return value.compareTo(numberBF) != 0;
		} else if (number instanceof RatioStructImpl) {
			return value.compareTo(((RatioStructImpl) number).value) != 0;
		}
		return number.isNotEqualTo(this);
	}

	@Override
	public IntegerStruct signum() {
		final int signum = BigFraction.ZERO.compareTo(value);
		return IntegerStruct.toLispInteger(signum);
	}

	@Override
	public RatioStruct negation() {
		final BigFraction negate = value.negate();
		return new RatioStructImpl(negate);
	}

	@Override
	public RationalStruct reciprocal() {
		return RationalStruct.toLispRational(denominator, numerator);
	}

	@Override
	public RealStruct exp() {
		final Aprational ap = ap();
		final Apfloat exp = ApfloatMath.exp(ap);
		return ApfloatUtils.toRealStruct(exp);
	}

	@Override
	public NumberStruct expt(final NumberStruct power) {
		final Aprational ap = ap();
		final Apcomplex powerAp = power.ap();
		if (powerAp instanceof Apfloat) {
			final Apfloat pow = ApfloatMath.pow(ap, (Apfloat) powerAp);
			return ApfloatUtils.toRealStruct(pow);
		}
		final Apcomplex pow = ApcomplexMath.pow(ap, powerAp);
		return ApfloatUtils.toNumberStruct(pow);
	}

	@Override
	public RealStruct log() {
		final Aprational ap = ap();
		final Apfloat log = ApfloatMath.log(ap);
		return ApfloatUtils.toRealStruct(log);
	}

	@Override
	public NumberStruct log(final NumberStruct base) {
		final Aprational ap = ap();
		final Apcomplex baseAp = base.ap();
		if (base instanceof RealStruct) {
			final Apfloat log = ApfloatMath.log(ap, (Apfloat) baseAp);
			return ApfloatUtils.toRealStruct(log);
		}
		final Apcomplex log = ApcomplexMath.log(ap, baseAp);
		return ApfloatUtils.toNumberStruct(log);
	}

	@Override
	public NumberStruct sqrt() {
		if (BigFraction.ZERO.compareTo(value) < 0) {
			final Aprational ap = new Aprational(new Apint(value.getNumerator()), new Apint(value.getDenominator()));
			final Apcomplex sqrt = ApcomplexMath.sqrt(ap);
			return ApfloatUtils.toNumberStruct(sqrt);
		}

		final Aprational ap = ap();
		final Apfloat sqrt = ApfloatMath.sqrt(ap);
		return ApfloatUtils.toRealStruct(sqrt);
	}

	@Override
	public RealStruct sin() {
		final Aprational ap = ap();
		final Apfloat sin = ApfloatMath.sin(ap);
		return ApfloatUtils.toRealStruct(sin);
	}

	@Override
	public RealStruct cos() {
		final Aprational ap = ap();
		final Apfloat cos = ApfloatMath.cos(ap);
		return ApfloatUtils.toRealStruct(cos);
	}

	@Override
	public RealStruct tan() {
		final Aprational ap = ap();
		final Apfloat tan = ApfloatMath.tan(ap);
		return ApfloatUtils.toRealStruct(tan);
	}

	@Override
	public RealStruct asin() {
		final Aprational ap = ap();
		final Apfloat asin = ApfloatMath.asin(ap);
		return ApfloatUtils.toRealStruct(asin);
	}

	@Override
	public RealStruct acos() {
		final Aprational ap = ap();
		final Apfloat acos = ApfloatMath.acos(ap);
		return ApfloatUtils.toRealStruct(acos);
	}

	@Override
	public RealStruct atan() {
		final Aprational ap = ap();
		final Apfloat atan = ApfloatMath.atan(ap);
		return ApfloatUtils.toRealStruct(atan);
	}

	@Override
	public RealStruct sinh() {
		final Aprational ap = ap();
		final Apfloat sinh = ApfloatMath.sinh(ap);
		return ApfloatUtils.toRealStruct(sinh);
	}

	@Override
	public RealStruct cosh() {
		final Aprational ap = ap();
		final Apfloat cosh = ApfloatMath.cosh(ap);
		return ApfloatUtils.toRealStruct(cosh);
	}

	@Override
	public RealStruct tanh() {
		final Aprational ap = ap();
		final Apfloat tanh = ApfloatMath.tanh(ap);
		return ApfloatUtils.toRealStruct(tanh);
	}

	@Override
	public RealStruct asinh() {
		final Aprational ap = ap();
		final Apfloat asinh = ApfloatMath.asinh(ap);
		return ApfloatUtils.toRealStruct(asinh);
	}

	@Override
	public RealStruct acosh() {
		final Aprational ap = ap();
		final Apfloat acosh = ApfloatMath.acosh(ap);
		return ApfloatUtils.toRealStruct(acosh);
	}

	@Override
	public RealStruct atanh() {
		final Aprational ap = ap();
		final Apfloat atanh = ApfloatMath.atanh(ap);
		return ApfloatUtils.toRealStruct(atanh);
	}
}
