package jcl.lang.internal;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.math.RoundingMode;
import java.util.function.Function;

import com.google.common.math.DoubleMath;
import jcl.lang.DoubleFloatStruct;
import jcl.lang.FloatStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.LongFloatStruct;
import jcl.lang.NumberStruct;
import jcl.lang.RationalStruct;
import jcl.lang.RealStruct;
import jcl.lang.SingleFloatStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.number.DecodeFloatResult;
import jcl.lang.number.QuotientRemainder;
import jcl.lang.statics.ReaderVariables;
import jcl.type.DoubleFloatType;
import jcl.type.FloatType;
import jcl.type.LongFloatType;
import jcl.type.ShortFloatType;
import jcl.type.SingleFloatType;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.util.ArithmeticUtils;
import org.apache.commons.math3.util.FastMath;
import org.apfloat.Apcomplex;
import org.apfloat.ApcomplexMath;
import org.apfloat.Apfloat;
import org.apfloat.ApfloatMath;
import org.apfloat.FixedPrecisionApfloatHelper;

@EqualsAndHashCode(callSuper = false)
public class DoubleFloatStructImpl extends BuiltInClassStruct implements DoubleFloatStruct, LongFloatStruct {

	/**
	 * The floating-point precision of a FloatStruct object.
	 */
	private static final int DOUBLE_PRECISION = 53;

	final double value;

	private static final FixedPrecisionApfloatHelper APFLOAT_HELPER = new FixedPrecisionApfloatHelper(DOUBLE_PRECISION);

	public DoubleFloatStructImpl(final double value) {
		super(SingleFloatType.INSTANCE, null, null);
		this.value = value;
	}

	public DoubleFloatStructImpl(final float value) {
		this((double) value);
	}

	@Override
	public DecodeFloatResult decodeFloat() {
		final long bits = Double.doubleToRawLongBits(value);
		final DecodedDouble decodedDouble = getDecodedDouble(bits);

		final long mantissa = decodedDouble.getMantissa();
		final double expt = ArithmeticUtils.pow(2, DOUBLE_PRECISION);
		final double significand = mantissa / expt;
		final DoubleFloatStruct significandFloat = DoubleFloatStruct.toLispFloat(significand);

		final long storedExponent = decodedDouble.getStoredExponent();
		// 1023 + 52 = 1075
		final long exponent = (storedExponent - 1075) + DOUBLE_PRECISION;
		final IntegerStruct exponentInteger = IntegerStruct.toLispInteger(exponent);

		final int sign = decodedDouble.getSign();
		final FloatStruct signFloat = (sign == 1) ? DoubleFloatStruct.ONE : DoubleFloatStruct.MINUS_ONE;

		return new DecodeFloatResult(significandFloat, exponentInteger, signFloat);
	}

	@Override
	public DecodeFloatResult integerDecodeFloat() {
		final long bits = Double.doubleToRawLongBits(value);
		final DecodedDouble decodedDouble = getDecodedDouble(bits);

		final long mantissa = decodedDouble.getMantissa();
		final IntegerStruct significandInteger = IntegerStruct.toLispInteger(mantissa);

		final long storedExponent = decodedDouble.getStoredExponent();
		// 1023 + 52 = 1075
		final long exponent = storedExponent - 1075;
		final IntegerStruct exponentInteger = IntegerStruct.toLispInteger(exponent);

		final int sign = decodedDouble.getSign();
		final IntegerStruct signInteger = (sign == 1) ? IntegerStruct.ONE : IntegerStruct.MINUS_ONE;

		return new DecodeFloatResult(significandInteger, exponentInteger, signInteger);
	}

	@Override
	public IntegerStruct floatPrecision() {
		return IntegerStruct.toLispInteger(DOUBLE_PRECISION);
	}

	@Override
	public FloatStruct floatSign() {
		final long bits = Double.doubleToRawLongBits(value);
		return (bits < 0) ? DoubleFloatStruct.MINUS_ONE : DoubleFloatStruct.ONE;
	}

	@SuppressWarnings("NumericCastThatLosesPrecision")
	@Override
	public float toJavaPFloat() {
		return (float) value;
	}

	@SuppressWarnings("NumericCastThatLosesPrecision")
	@Override
	public Float toJavaFloat() {
		return (float) value;
	}

	@Override
	public double toJavaPDouble() {
		return value;
	}

	@Override
	public Double toJavaDouble() {
		return value;
	}

	@Override
	public BigDecimal toJavaBigDecimal() {
		return BigDecimal.valueOf(value);
	}

	/**
	 * Decodes the double by the provided {@code long} bits into its sign, exponent, and mantissa according to the
	 * details in the JVM spec section 4.4.5.
	 *
	 * @param bits
	 * 		the {@code long} bits representing the {@code double} value
	 *
	 * @return the {@link DecodedDouble} wrapping the decoded sign, exponent, and mantissa values
	 *
	 * @see <a href="https://docs.oracle.com/javase/8/docs/api/java/lang/Double.html">Java Double</a>
	 */
	private static DecodedDouble getDecodedDouble(final long bits) {
		final int sign = ((bits >> 63) == 0) ? 1 : -1;
		final long exponent = (bits >> 52) & 0x7ffL;
		final long mantissa = (exponent == 0) ?
		                      ((bits & 0xfffffffffffffL) << 1) :
		                      ((bits & 0xfffffffffffffL) | 0x10000000000000L);
		return new DecodedDouble(mantissa, exponent, sign);
	}

	/**
	 * Decoded wrapper for {@code double} sign, exponent, and mantissa values.
	 */
	@RequiredArgsConstructor
	@Getter
	private static final class DecodedDouble {

		/**
		 * The part of the {@code double} that represents the significant digits.
		 */
		private final long mantissa;

		/**
		 * The part of the {@code double} that represents the exponent.
		 */
		private final long storedExponent;

		/**
		 * The part of the {@code double} that represents the sign bit.
		 */
		private final int sign;
	}

	/*
	REAL-STRUCT
	 */

	@Override
	public boolean isLessThan(final RealStruct real) {
		if (real instanceof SingleFloatStructImpl) {
			return Double.compare(value, ((SingleFloatStructImpl) real).value) < 0;
		} else if (real instanceof DoubleFloatStructImpl) {
			return Double.compare(value, ((DoubleFloatStructImpl) real).value) < 0;
		}
		return rational().isLessThan(real);
	}

	@Override
	public boolean isGreaterThan(final RealStruct real) {
		if (real instanceof SingleFloatStructImpl) {
			return Double.compare(value, ((SingleFloatStructImpl) real).value) > 0;
		} else if (real instanceof DoubleFloatStructImpl) {
			return Double.compare(value, ((DoubleFloatStructImpl) real).value) > 0;
		}
		return rational().isLessThan(real);
	}

	@Override
	public boolean isLessThanOrEqualTo(final RealStruct real) {
		if (real instanceof SingleFloatStructImpl) {
			return Double.compare(value, ((SingleFloatStructImpl) real).value) <= 0;
		} else if (real instanceof DoubleFloatStructImpl) {
			return Double.compare(value, ((DoubleFloatStructImpl) real).value) <= 0;
		}
		return rational().isLessThan(real);
	}

	@Override
	public boolean isGreaterThanOrEqualTo(final RealStruct real) {
		if (real instanceof SingleFloatStructImpl) {
			return Double.compare(value, ((SingleFloatStructImpl) real).value) >= 0;
		} else if (real instanceof DoubleFloatStructImpl) {
			return Double.compare(value, ((DoubleFloatStructImpl) real).value) >= 0;
		}
		return rational().isLessThan(real);
	}

	@Override
	public boolean plusp() {
		return Double.compare(value, 0.0D) > 0;
	}

	@Override
	public boolean minusp() {
		return Double.compare(value, 0.0D) < 0;
	}

	@Override
	public RationalStruct rational() {
		final BigFraction bigFraction = new BigFraction(value);
		final BigFraction bigFractionReduced = bigFraction.reduce();

		final BigInteger numerator = bigFractionReduced.getNumerator();
		final BigInteger denominator = bigFractionReduced.getDenominator();

		if (BigInteger.ONE.equals(denominator)) {
			return IntegerStruct.toLispInteger(numerator);
		}

		return new RatioStructImpl(new BigFraction(numerator, denominator));
	}

	@Override
	public FloatStruct floatingPoint(final FloatStruct prototype) {
		if (prototype instanceof DoubleFloatStructImpl) {
			return this;
		} else {
			return SingleFloatStruct.toLispFloat(value);
		}
	}

	/**
	 * Calculates the quotient remainder using the provided {@code operation}. The resulting quotient will be created
	 * using the provided {@code quotientCreator}.
	 *
	 * @param operation
	 * 		the quotient/remainder operation to perform
	 * @param quotientCreator
	 * 		the quotient generator
	 *
	 * @return a {@link QuotientRemainder} containing the results of the operation
	 */
	private QuotientRemainder calculateQuotientRemainder(final Function<Double, Long> operation,
	                                                     final Function<Long, RealStruct> quotientCreator) {
		final long quotient = operation.apply(value);
		final double remainder = value - quotient;

		return new QuotientRemainder(
				quotientCreator.apply(quotient),
				SingleFloatStruct.toLispFloat(remainder)
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

		if (divisor instanceof FixnumStructImpl) {
			final int divisorValue = ((FixnumStructImpl) divisor).value;
			final double divide = value / divisorValue;

			final long quotient = operation.apply(divide);
			final double remainder = value - (quotient * divisorValue);

			return new QuotientRemainder(
					quotientCreator.apply(quotient),
					DoubleFloatStruct.toLispFloat(remainder)
			);
		} else if (divisor instanceof LongnumStructImpl) {
			final long divisorValue = ((LongnumStructImpl) divisor).value;
			final double divide = value / divisorValue;

			final long quotient = operation.apply(divide);
			final double remainder = value - (quotient * divisorValue);

			return new QuotientRemainder(
					quotientCreator.apply(quotient),
					DoubleFloatStruct.toLispFloat(remainder)
			);
		} else if (divisor instanceof BignumStructImpl) {
			final float divisorValue = ((BignumStructImpl) divisor).value.floatValue();
			final double divide = value / divisorValue;

			final long quotient = operation.apply(divide);
			final double remainder = value - (quotient * divisorValue);

			return new QuotientRemainder(
					quotientCreator.apply(quotient),
					DoubleFloatStruct.toLispFloat(remainder)
			);
		} else if (divisor instanceof RatioStructImpl) {
			final float divisorValue = ((RatioStructImpl) divisor).value.floatValue();
			final double divide = value / divisorValue;

			final long quotient = operation.apply(divide);
			final double remainder = value - (quotient * divisorValue);

			return new QuotientRemainder(
					quotientCreator.apply(quotient),
					DoubleFloatStruct.toLispFloat(remainder)
			);
		} else if (divisor instanceof SingleFloatStructImpl) {
			final float divisorValue = ((SingleFloatStructImpl) divisor).value;
			final double divide = value / divisorValue;

			final long quotient = operation.apply(divide);
			final double remainder = value - (quotient * divisorValue);

			return new QuotientRemainder(
					quotientCreator.apply(quotient),
					DoubleFloatStruct.toLispFloat(remainder)
			);
		} else if (divisor instanceof DoubleFloatStructImpl) {
			final double divisorValue = ((DoubleFloatStructImpl) divisor).value;
			final double divide = value / divisorValue;

			final long quotient = operation.apply(divide);
			final double remainder = value - (quotient * divisorValue);

			return new QuotientRemainder(
					quotientCreator.apply(quotient),
					DoubleFloatStruct.toLispFloat(remainder)
			);
		} else {
			final double divisorValue = divisor.ap().doubleValue();
			final double divide = value / divisorValue;

			final long quotient = operation.apply(divide);
			final double remainder = value - (quotient * divisorValue);

			return new QuotientRemainder(
					quotientCreator.apply(quotient),
					DoubleFloatStruct.toLispFloat(remainder)
			);
		}
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
		                                  DoubleFloatStruct::toLispFloat);
	}

	@SuppressWarnings("NumericCastThatLosesPrecision")
	@Override
	public QuotientRemainder ffloor(final RealStruct divisor) {
		return calculateQuotientRemainder(divisor, val -> (long) StrictMath.floor(val),
		                                  DoubleFloatStruct::toLispFloat);
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
		                                  DoubleFloatStruct::toLispFloat);
	}

	@SuppressWarnings("NumericCastThatLosesPrecision")
	@Override
	public QuotientRemainder fceiling(final RealStruct divisor) {
		return calculateQuotientRemainder(divisor, val -> (long) StrictMath.ceil(val),
		                                  DoubleFloatStruct::toLispFloat);
	}

	@Override
	public QuotientRemainder truncate() {
		return calculateQuotientRemainder(val -> DoubleMath.roundToLong(value, RoundingMode.DOWN),
		                                  IntegerStruct::toLispInteger);
	}

	@Override
	public QuotientRemainder truncate(final RealStruct divisor) {
		return calculateQuotientRemainder(divisor, val -> DoubleMath.roundToLong(value, RoundingMode.DOWN),
		                                  IntegerStruct::toLispInteger);
	}

	@Override
	public QuotientRemainder ftruncate() {
		return calculateQuotientRemainder(val -> DoubleMath.roundToLong(value, RoundingMode.DOWN),
		                                  DoubleFloatStruct::toLispFloat);
	}

	@Override
	public QuotientRemainder ftruncate(final RealStruct divisor) {
		return calculateQuotientRemainder(divisor, val -> DoubleMath.roundToLong(value, RoundingMode.DOWN),
		                                  DoubleFloatStruct::toLispFloat);
	}

	@Override
	public QuotientRemainder round() {
		return calculateQuotientRemainder(val -> DoubleMath.roundToLong(value, RoundingMode.HALF_EVEN),
		                                  IntegerStruct::toLispInteger);
	}

	@Override
	public QuotientRemainder round(final RealStruct divisor) {
		return calculateQuotientRemainder(divisor, val -> DoubleMath.roundToLong(value, RoundingMode.HALF_EVEN),
		                                  IntegerStruct::toLispInteger);
	}

	@Override
	public QuotientRemainder fround() {
		return calculateQuotientRemainder(val -> DoubleMath.roundToLong(value, RoundingMode.HALF_EVEN),
		                                  DoubleFloatStruct::toLispFloat);
	}

	@Override
	public QuotientRemainder fround(final RealStruct divisor) {
		return calculateQuotientRemainder(divisor, val -> DoubleMath.roundToLong(value, RoundingMode.HALF_EVEN),
		                                  DoubleFloatStruct::toLispFloat);
	}

	@Override
	public RealStruct atan(final RealStruct real) {
		if (real instanceof FixnumStructImpl) {
			final double atan2 = StrictMath.atan2(value, ((FixnumStructImpl) real).value);
			return DoubleFloatStruct.toLispFloat(atan2);
		} else if (real instanceof LongnumStructImpl) {
			final double atan2 = StrictMath.atan2(value, ((LongnumStructImpl) real).value);
			return DoubleFloatStruct.toLispFloat(atan2);
		} else if (real instanceof SingleFloatStructImpl) {
			final double atan2 = StrictMath.atan2(value, ((SingleFloatStructImpl) real).value);
			return DoubleFloatStruct.toLispFloat(atan2);
		} else if (real instanceof DoubleFloatStructImpl) {
			final double atan2 = StrictMath.atan2(value, ((DoubleFloatStructImpl) real).value);
			return DoubleFloatStruct.toLispFloat(atan2);
		}
		final Apfloat ap = new Apfloat(value);
		final Apfloat realAp = real.ap();
		final Apfloat atan2 = ApfloatMath.atan2(ap, realAp);
		return ApfloatUtils.toRealStruct(atan2);
	}

	/*
	NUMBER-STRUCT
	 */

	@Override
	public Apfloat ap() {
		return new Apfloat(value);
	}

	@Override
	public FloatStruct abs() {
		return DoubleFloatStruct.toLispFloat(Math.abs(value));
	}

	@Override
	public boolean zerop() {
		return Double.compare(value, 0.0D) == 0;
	}

	@Override
	public NumberStruct add(final NumberStruct number) {
		if (number instanceof FixnumStructImpl) {
			final double add = value + ((FixnumStructImpl) number).value;
			return DoubleFloatStruct.toLispFloat(add);
		} else if (number instanceof LongnumStructImpl) {
			final double add = value + ((LongnumStructImpl) number).value;
			return DoubleFloatStruct.toLispFloat(add);
		} else if (number instanceof BignumStructImpl) {
			final BigDecimal bigDecimal1 = BigDecimal.valueOf(value);
			final BigDecimal bigDecimal2 = new BigDecimal(((BignumStructImpl) number).value);
			final BigDecimal add = bigDecimal1.add(bigDecimal2);
			return DoubleFloatStruct.toLispFloat(add.floatValue());
		} else if (number instanceof RatioStructImpl) {
			final double add = value + ((RatioStructImpl) number).value.floatValue();
			return DoubleFloatStruct.toLispFloat(add);
		} else if (number instanceof SingleFloatStructImpl) {
			final double add = value + ((SingleFloatStructImpl) number).value;
			return DoubleFloatStruct.toLispFloat(add);
		} else if (number instanceof DoubleFloatStructImpl) {
			final double add = value + ((DoubleFloatStructImpl) number).value;
			return DoubleFloatStruct.toLispFloat(add);
		}
		return number.add(this);
	}

	@Override
	public NumberStruct subtract(final NumberStruct number) {
		if (number instanceof FixnumStructImpl) {
			final double subtract = value - ((FixnumStructImpl) number).value;
			return DoubleFloatStruct.toLispFloat(subtract);
		} else if (number instanceof LongnumStructImpl) {
			final double subtract = value - ((LongnumStructImpl) number).value;
			return DoubleFloatStruct.toLispFloat(subtract);
		} else if (number instanceof BignumStructImpl) {
			final BigDecimal bigDecimal1 = BigDecimal.valueOf(value);
			final BigDecimal bigDecimal2 = new BigDecimal(((BignumStructImpl) number).value);
			final BigDecimal subtract = bigDecimal1.subtract(bigDecimal2);
			return DoubleFloatStruct.toLispFloat(subtract.floatValue());
		} else if (number instanceof RatioStructImpl) {
			final double subtract = value - ((RatioStructImpl) number).value.floatValue();
			return DoubleFloatStruct.toLispFloat(subtract);
		} else if (number instanceof SingleFloatStructImpl) {
			final double subtract = value - ((SingleFloatStructImpl) number).value;
			return DoubleFloatStruct.toLispFloat(subtract);
		} else if (number instanceof DoubleFloatStructImpl) {
			final double subtract = value - ((DoubleFloatStructImpl) number).value;
			return DoubleFloatStruct.toLispFloat(subtract);
		}
		final Apfloat ap = new Apfloat(value);
		final Apcomplex numberAp = number.ap();
		final Apcomplex subtract = ap.subtract(numberAp);
		return ApfloatUtils.toNumberStruct(subtract);
	}

	@Override
	public NumberStruct multiply(final NumberStruct number) {
		if (number instanceof FixnumStructImpl) {
			final double multiply = value * ((FixnumStructImpl) number).value;
			return DoubleFloatStruct.toLispFloat(multiply);
		} else if (number instanceof LongnumStructImpl) {
			final double multiply = value * ((LongnumStructImpl) number).value;
			return DoubleFloatStruct.toLispFloat(multiply);
		} else if (number instanceof BignumStructImpl) {
			final BigDecimal bigDecimal1 = BigDecimal.valueOf(value);
			final BigDecimal bigDecimal2 = new BigDecimal(((BignumStructImpl) number).value);
			final BigDecimal multiply = bigDecimal1.multiply(bigDecimal2);
			return DoubleFloatStruct.toLispFloat(multiply.floatValue());
		} else if (number instanceof RatioStructImpl) {
			final double multiply = value * ((RatioStructImpl) number).value.floatValue();
			return DoubleFloatStruct.toLispFloat(multiply);
		} else if (number instanceof SingleFloatStructImpl) {
			final double multiply = value * ((SingleFloatStructImpl) number).value;
			return DoubleFloatStruct.toLispFloat(multiply);
		} else if (number instanceof DoubleFloatStructImpl) {
			final double multiply = value * ((DoubleFloatStructImpl) number).value;
			return DoubleFloatStruct.toLispFloat(multiply);
		}
		return number.multiply(this);
	}

	@Override
	public NumberStruct divide(final NumberStruct number) {
		if (number instanceof FixnumStructImpl) {
			final double divide = value / ((FixnumStructImpl) number).value;
			return DoubleFloatStruct.toLispFloat(divide);
		} else if (number instanceof LongnumStructImpl) {
			final double divide = value / ((LongnumStructImpl) number).value;
			return DoubleFloatStruct.toLispFloat(divide);
		} else if (number instanceof BignumStructImpl) {
			final BigDecimal bigDecimal1 = BigDecimal.valueOf(value);
			final BigDecimal bigDecimal2 = new BigDecimal(((BignumStructImpl) number).value);
			final BigDecimal divide = bigDecimal1.divide(bigDecimal2, MathContext.DECIMAL64);
			return DoubleFloatStruct.toLispFloat(divide.floatValue());
		} else if (number instanceof RatioStructImpl) {
			final double divide = value / ((RatioStructImpl) number).value.floatValue();
			return DoubleFloatStruct.toLispFloat(divide);
		} else if (number instanceof SingleFloatStructImpl) {
			final double divide = value / ((SingleFloatStructImpl) number).value;
			return DoubleFloatStruct.toLispFloat(divide);
		} else if (number instanceof DoubleFloatStructImpl) {
			final double divide = value / ((DoubleFloatStructImpl) number).value;
			return DoubleFloatStruct.toLispFloat(divide);
		}
		final Apfloat ap = new Apfloat(value);
		final Apcomplex numberAp = number.ap();
		final Apcomplex divide = ap.divide(numberAp);
		return ApfloatUtils.toNumberStruct(divide);
	}

	@Override
	public boolean isEqualTo(final NumberStruct number) {
		if (number instanceof SingleFloatStructImpl) {
			return Double.compare(value, ((SingleFloatStructImpl) number).value) == 0;
		} else if (number instanceof DoubleFloatStructImpl) {
			return Double.compare(value, ((DoubleFloatStructImpl) number).value) == 0;
		} else if (number instanceof RationalStruct) {
			return rational().isEqualTo(number);
		}
		return number.isEqualTo(this);
	}

	@Override
	public boolean isNotEqualTo(final NumberStruct number) {
		if (number instanceof SingleFloatStructImpl) {
			return Double.compare(value, ((SingleFloatStructImpl) number).value) != 0;
		} else if (number instanceof DoubleFloatStructImpl) {
			return Double.compare(value, ((DoubleFloatStructImpl) number).value) != 0;
		} else if (number instanceof RationalStruct) {
			return rational().isNotEqualTo(number);
		}
		return number.isNotEqualTo(this);
	}

	@Override
	public FloatStruct signum() {
		final int compare = Double.compare(value, 0.0D);
		if (compare == 0) {
			return this;
		}
		if (compare > 0) {
			return DoubleFloatStruct.ONE;
		}
		return DoubleFloatStruct.MINUS_ONE;
	}

	@Override
	public FloatStruct negation() {
		return DoubleFloatStruct.toLispFloat(-value);
	}

	@Override
	public FloatStruct reciprocal() {
		return DoubleFloatStruct.toLispFloat(1 / value);
	}

	@Override
	public RealStruct exp() {
		final double exp = StrictMath.exp(value);
		return DoubleFloatStruct.toLispFloat(exp);
	}

	@Override
	public NumberStruct expt(final NumberStruct power) {
		final Apfloat ap = new Apfloat(value);
		if (power instanceof RealStruct) {
			final Apfloat powerAp = ((RealStruct) power).ap();
			final Apfloat pow = ApfloatMath.pow(ap, powerAp);
			return ApfloatUtils.toRealStruct(pow);
		}
		final Apcomplex powerAp = power.ap();
		final Apcomplex pow = ApcomplexMath.pow(ap, powerAp);
		return ApfloatUtils.toNumberStruct(pow);
	}

	@Override
	public RealStruct log() {
		final double log = StrictMath.log(value);
		return DoubleFloatStruct.toLispFloat(log);
	}

	@Override
	public NumberStruct log(final NumberStruct base) {
		final Apfloat ap = new Apfloat(value);
		if (base instanceof RealStruct) {
			final Apfloat baseAp = ((RealStruct) base).ap();
			final Apfloat log = ApfloatMath.log(ap, baseAp);
			return ApfloatUtils.toRealStruct(log);
		}
		final Apcomplex baseAp = base.ap();
		final Apcomplex log = ApcomplexMath.log(ap, baseAp);
		return ApfloatUtils.toNumberStruct(log);
	}

	@Override
	public NumberStruct sqrt() {
		if (Double.compare(value, 0.0D) < 0) {
			final Apfloat ap = new Apfloat(value);
			final Apcomplex sqrt = ApcomplexMath.sqrt(ap);
			return ApfloatUtils.toNumberStruct(sqrt);
		}

		final double sqrt = StrictMath.sqrt(value);
		return DoubleFloatStruct.toLispFloat(sqrt);
	}

	@Override
	public RealStruct sin() {
		final double sin = StrictMath.sin(value);
		return DoubleFloatStruct.toLispFloat(sin);
	}

	@Override
	public RealStruct cos() {
		final double cos = StrictMath.cos(value);
		return DoubleFloatStruct.toLispFloat(cos);
	}

	@Override
	public RealStruct tan() {
		final double tan = StrictMath.tan(value);
		return DoubleFloatStruct.toLispFloat(tan);
	}

	@Override
	public RealStruct asin() {
		final double asin = StrictMath.asin(value);
		return DoubleFloatStruct.toLispFloat(asin);
	}

	@Override
	public RealStruct acos() {
		final double acos = StrictMath.acos(value);
		return DoubleFloatStruct.toLispFloat(acos);
	}

	@Override
	public RealStruct atan() {
		final double atan = StrictMath.atan(value);
		return DoubleFloatStruct.toLispFloat(atan);
	}

	@Override
	public RealStruct sinh() {
		final double sinh = StrictMath.sinh(value);
		return DoubleFloatStruct.toLispFloat(sinh);
	}

	@Override
	public RealStruct cosh() {
		final double cosh = StrictMath.cosh(value);
		return DoubleFloatStruct.toLispFloat(cosh);
	}

	@Override
	public RealStruct tanh() {
		final double tanh = StrictMath.tanh(value);
		return DoubleFloatStruct.toLispFloat(tanh);
	}

	@Override
	public RealStruct asinh() {
		final double asinh = FastMath.asinh(value);
		return DoubleFloatStruct.toLispFloat(asinh);
	}

	@Override
	public RealStruct acosh() {
		final double acosh = FastMath.acosh(value);
		return DoubleFloatStruct.toLispFloat(acosh);
	}

	@Override
	public RealStruct atanh() {
		final double atanh = FastMath.atanh(value);
		return DoubleFloatStruct.toLispFloat(atanh);
	}

	/*
		ToString
	 */

	@Override
	public String toString() {

		final FloatType floatType = (FloatType) getType();
		final FloatType defaultFloatFormat = ReaderVariables.READ_DEFAULT_FLOAT_FORMAT.getVariableValue();

		String floatString = String.valueOf(value);
		if (floatType.isNotOfType(defaultFloatFormat)) {
			if (floatType.isOfType(ShortFloatType.INSTANCE)) {
				floatString = floatString.replace('E', 'S');
			} else if (floatType.isOfType(SingleFloatType.INSTANCE)) {
				floatString = floatString.replace('E', 'F');
			} else if (floatType.isOfType(DoubleFloatType.INSTANCE)) {
				floatString = floatString.replace('E', 'D');
			} else if (floatType.isOfType(LongFloatType.INSTANCE)) {
				floatString = floatString.replace('E', 'L');
			}
		}

		return floatString;
	}
}
