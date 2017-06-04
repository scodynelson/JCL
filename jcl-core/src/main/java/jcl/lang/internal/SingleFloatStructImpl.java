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
import jcl.lang.NumberStruct;
import jcl.lang.RationalStruct;
import jcl.lang.RealStruct;
import jcl.lang.ShortFloatStruct;
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
public class SingleFloatStructImpl extends BuiltInClassStruct implements SingleFloatStruct, ShortFloatStruct {

	/**
	 * The floating-point precision of a FloatStruct object.
	 */
	public static final int FLOAT_PRECISION = 24;

	/**
	 * The float value.
	 */
	final float value;

	private static final FixedPrecisionApfloatHelper APFLOAT_HELPER = new FixedPrecisionApfloatHelper(FLOAT_PRECISION);

	/**
	 * Public constructor.
	 *
	 * @param value
	 * 		the float value
	 */
	public SingleFloatStructImpl(final float value) {
		super(SingleFloatType.INSTANCE, null, null);
		this.value = value;
	}

	@Override
	public DecodeFloatResult decodeFloat() {
		final int bits = Float.floatToRawIntBits(value);
		final DecodedFloat decodedFloat = getDecodedFloat(bits);

		final int mantissa = decodedFloat.getMantissa();
		final float expt = ArithmeticUtils.pow(2, FLOAT_PRECISION);
		final float significand = mantissa / expt;
		final SingleFloatStruct significandFloat = SingleFloatStruct.toLispFloat(significand);

		final int storedExponent = decodedFloat.getStoredExponent();
		// 127 + 23 = 150
		final int exponent = (storedExponent - 150) + FLOAT_PRECISION;
		final IntegerStruct exponentInteger = IntegerStruct.toLispInteger(exponent);

		final int sign = decodedFloat.getSign();
		final FloatStruct signFloat = (sign == 1) ? SingleFloatStruct.ONE : SingleFloatStruct.MINUS_ONE;

		return new DecodeFloatResult(significandFloat, exponentInteger, signFloat);
	}

	@Override
	public DecodeFloatResult integerDecodeFloat() {
		final int bits = Float.floatToRawIntBits(value);
		final DecodedFloat decodedFloat = getDecodedFloat(bits);

		final int mantissa = decodedFloat.getMantissa();
		final IntegerStruct significandInteger = IntegerStruct.toLispInteger(mantissa);

		final int storedExponent = decodedFloat.getStoredExponent();
		// 127 + 23 = 150
		final int exponent = storedExponent - 150;
		final IntegerStruct exponentInteger = IntegerStruct.toLispInteger(exponent);

		final int sign = decodedFloat.getSign();
		final IntegerStruct signInteger = (sign == 1) ? IntegerStruct.ONE_NEW : IntegerStruct.MINUS_ONE_NEW;

		return new DecodeFloatResult(significandInteger, exponentInteger, signInteger);
	}

	@Override
	public IntegerStruct floatPrecision() {
		return IntegerStruct.toLispInteger(FLOAT_PRECISION);
	}

	@Override
	public FloatStruct floatSign() {
		final int bits = Float.floatToRawIntBits(value);
		return (bits < 0) ? SingleFloatStruct.MINUS_ONE : SingleFloatStruct.ONE;
	}

	@Override
	public float toJavaPFloat() {
		return value;
	}

	@Override
	public Float toJavaFloat() {
		return value;
	}

	@Override
	public double toJavaPDouble() {
		return value;
	}

	@Override
	public Double toJavaDouble() {
		return (double) value;
	}

	@Override
	public BigDecimal toJavaBigDecimal() {
		return BigDecimal.valueOf(value);
	}

	/**
	 * Decodes the float by the provided {@code int} bits into its sign, exponent, and mantissa according to the
	 * details in the JVM spec section 4.4.5.
	 *
	 * @param bits
	 * 		the {@code int} bits representing the {@code float} value
	 *
	 * @return the {@link DecodedFloat} wrapping the decoded sign, exponent, and mantissa values
	 *
	 * @see <a href="https://docs.oracle.com/javase/8/docs/api/java/lang/Float.html">Java Float</a>
	 */
	private static DecodedFloat getDecodedFloat(final int bits) {
		final int sign = ((bits >> 31) == 0) ? 1 : -1;
		final int exponent = (bits >> 23) & 0xff;
		final int mantissa = (exponent == 0) ?
		                     ((bits & 0x7fffff) << 1) :
		                     ((bits & 0x7fffff) | 0x800000);
		return new DecodedFloat(mantissa, exponent, sign);
	}

	/**
	 * Decoded wrapper for {@code float} sign, exponent, and mantissa values.
	 */
	@RequiredArgsConstructor
	@Getter
	private static final class DecodedFloat {

		/**
		 * The part of the {@code float} that represents the significant digits.
		 */
		private final int mantissa;

		/**
		 * The part of the {@code float} that represents the exponent.
		 */
		private final int storedExponent;

		/**
		 * The part of the {@code float} that represents the sign bit.
		 */
		private final int sign;
	}

	/*
	REAL-STRUCT
	 */

	@Override
	public boolean isLessThan(final RealStruct real) {
		if (real instanceof SingleFloatStructImpl) {
			return Float.compare(value, ((SingleFloatStructImpl) real).value) < 0;
		} else if (real instanceof DoubleFloatStructImpl) {
			return Double.compare(value, ((DoubleFloatStructImpl) real).value) < 0;
		}
		return rational().isLessThan(real);
	}

	@Override
	public boolean isGreaterThan(final RealStruct real) {
		if (real instanceof SingleFloatStructImpl) {
			return Float.compare(value, ((SingleFloatStructImpl) real).value) > 0;
		} else if (real instanceof DoubleFloatStructImpl) {
			return Double.compare(value, ((DoubleFloatStructImpl) real).value) > 0;
		}
		return rational().isLessThan(real);
	}

	@Override
	public boolean isLessThanOrEqualTo(final RealStruct real) {
		if (real instanceof SingleFloatStructImpl) {
			return Float.compare(value, ((SingleFloatStructImpl) real).value) <= 0;
		} else if (real instanceof DoubleFloatStructImpl) {
			return Double.compare(value, ((DoubleFloatStructImpl) real).value) <= 0;
		}
		return rational().isLessThan(real);
	}

	@Override
	public boolean isGreaterThanOrEqualTo(final RealStruct real) {
		if (real instanceof SingleFloatStructImpl) {
			return Float.compare(value, ((SingleFloatStructImpl) real).value) >= 0;
		} else if (real instanceof DoubleFloatStructImpl) {
			return Double.compare(value, ((DoubleFloatStructImpl) real).value) >= 0;
		}
		return rational().isLessThan(real);
	}

	@Override
	public boolean plusp() {
		return Float.compare(value, 0.0F) > 0;
	}

	@Override
	public boolean minusp() {
		return Float.compare(value, 0.0F) < 0;
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
		if (prototype instanceof SingleFloatStructImpl) {
			return this;
		} else {
			return DoubleFloatStruct.toLispFloat(value);
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
	private QuotientRemainder calculateQuotientRemainder(final Function<Float, Long> operation,
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
					SingleFloatStruct.toLispFloat(remainder)
			);
		} else if (divisor instanceof LongnumStructImpl) {
			final long divisorValue = ((LongnumStructImpl) divisor).value;
			final double divide = value / divisorValue;

			final long quotient = operation.apply(divide);
			final double remainder = value - (quotient * divisorValue);

			return new QuotientRemainder(
					quotientCreator.apply(quotient),
					SingleFloatStruct.toLispFloat(remainder)
			);
		} else if (divisor instanceof BignumStructImpl) {
			final float divisorValue = ((BignumStructImpl) divisor).value.floatValue();
			final double divide = value / divisorValue;

			final long quotient = operation.apply(divide);
			final double remainder = value - (quotient * divisorValue);

			return new QuotientRemainder(
					quotientCreator.apply(quotient),
					SingleFloatStruct.toLispFloat(remainder)
			);
		} else if (divisor instanceof RatioStructImpl) {
			final float divisorValue = ((RatioStructImpl) divisor).value.floatValue();
			final double divide = value / divisorValue;

			final long quotient = operation.apply(divide);
			final double remainder = value - (quotient * divisorValue);

			return new QuotientRemainder(
					quotientCreator.apply(quotient),
					SingleFloatStruct.toLispFloat(remainder)
			);
		} else if (divisor instanceof SingleFloatStructImpl) {
			final float divisorValue = ((SingleFloatStructImpl) divisor).value;
			final double divide = value / divisorValue;

			final long quotient = operation.apply(divide);
			final double remainder = value - (quotient * divisorValue);

			return new QuotientRemainder(
					quotientCreator.apply(quotient),
					SingleFloatStruct.toLispFloat(remainder)
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
					SingleFloatStruct.toLispFloat(remainder)
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
		       ? SingleFloatStruct::toLispFloat
		       : DoubleFloatStruct::toLispFloat;
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
		                                  SingleFloatStruct::toLispFloat);
	}

	@Override
	public QuotientRemainder ftruncate(final RealStruct divisor) {
		return calculateQuotientRemainder(divisor, val -> DoubleMath.roundToLong(value, RoundingMode.DOWN),
		                                  toLispFloat(divisor));
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
		                                  SingleFloatStruct::toLispFloat);
	}

	@Override
	public QuotientRemainder fround(final RealStruct divisor) {
		return calculateQuotientRemainder(divisor, val -> DoubleMath.roundToLong(value, RoundingMode.HALF_EVEN),
		                                  toLispFloat(divisor));
	}

	@Override
	public RealStruct atan(final RealStruct real) {
		if (real instanceof FixnumStructImpl) {
			final double atan2 = StrictMath.atan2(value, ((FixnumStructImpl) real).value);
			return SingleFloatStruct.toLispFloat(atan2);
		} else if (real instanceof LongnumStructImpl) {
			final double atan2 = StrictMath.atan2(value, ((LongnumStructImpl) real).value);
			return SingleFloatStruct.toLispFloat(atan2);
		} else if (real instanceof SingleFloatStructImpl) {
			final double atan2 = StrictMath.atan2(value, ((SingleFloatStructImpl) real).value);
			return SingleFloatStruct.toLispFloat(atan2);
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
		return SingleFloatStruct.toLispFloat(Math.abs(value));
	}

	@Override
	public boolean zerop() {
		return Float.compare(value, 0.0F) == 0;
	}

	@Override
	public NumberStruct add(final NumberStruct number) {
		if (number instanceof FixnumStructImpl) {
			final float add = value + ((FixnumStructImpl) number).value;
			return SingleFloatStruct.toLispFloat(add);
		} else if (number instanceof LongnumStructImpl) {
			final float add = value + ((LongnumStructImpl) number).value;
			return SingleFloatStruct.toLispFloat(add);
		} else if (number instanceof BignumStructImpl) {
			final BigDecimal bigDecimal1 = BigDecimal.valueOf(value);
			final BigDecimal bigDecimal2 = new BigDecimal(((BignumStructImpl) number).value);
			final BigDecimal add = bigDecimal1.add(bigDecimal2);
			return SingleFloatStruct.toLispFloat(add.floatValue());
		} else if (number instanceof RatioStructImpl) {
			final float add = value + ((RatioStructImpl) number).value.floatValue();
			return SingleFloatStruct.toLispFloat(add);
		} else if (number instanceof SingleFloatStructImpl) {
			final float add = value + ((SingleFloatStructImpl) number).value;
			return SingleFloatStruct.toLispFloat(add);
		} else if (number instanceof DoubleFloatStructImpl) {
			final double add = value + ((DoubleFloatStructImpl) number).value;
			return DoubleFloatStruct.toLispFloat(add);
		}
		return number.add(this);
	}

	@Override
	public NumberStruct subtract(final NumberStruct number) {
		if (number instanceof FixnumStructImpl) {
			final float subtract = value - ((FixnumStructImpl) number).value;
			return SingleFloatStruct.toLispFloat(subtract);
		} else if (number instanceof LongnumStructImpl) {
			final float subtract = value - ((LongnumStructImpl) number).value;
			return SingleFloatStruct.toLispFloat(subtract);
		} else if (number instanceof BignumStructImpl) {
			final BigDecimal bigDecimal1 = BigDecimal.valueOf(value);
			final BigDecimal bigDecimal2 = new BigDecimal(((BignumStructImpl) number).value);
			final BigDecimal subtract = bigDecimal1.subtract(bigDecimal2);
			return SingleFloatStruct.toLispFloat(subtract.floatValue());
		} else if (number instanceof RatioStructImpl) {
			final float subtract = value - ((RatioStructImpl) number).value.floatValue();
			return SingleFloatStruct.toLispFloat(subtract);
		} else if (number instanceof SingleFloatStructImpl) {
			final float subtract = value - ((SingleFloatStructImpl) number).value;
			return SingleFloatStruct.toLispFloat(subtract);
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
			final float multiply = value * ((FixnumStructImpl) number).value;
			return SingleFloatStruct.toLispFloat(multiply);
		} else if (number instanceof LongnumStructImpl) {
			final float multiply = value * ((LongnumStructImpl) number).value;
			return SingleFloatStruct.toLispFloat(multiply);
		} else if (number instanceof BignumStructImpl) {
			final BigDecimal bigDecimal1 = BigDecimal.valueOf(value);
			final BigDecimal bigDecimal2 = new BigDecimal(((BignumStructImpl) number).value);
			final BigDecimal multiply = bigDecimal1.multiply(bigDecimal2);
			return SingleFloatStruct.toLispFloat(multiply.floatValue());
		} else if (number instanceof RatioStructImpl) {
			final float multiply = value * ((RatioStructImpl) number).value.floatValue();
			return SingleFloatStruct.toLispFloat(multiply);
		} else if (number instanceof SingleFloatStructImpl) {
			final float multiply = value * ((SingleFloatStructImpl) number).value;
			return SingleFloatStruct.toLispFloat(multiply);
		} else if (number instanceof DoubleFloatStructImpl) {
			final double multiply = value * ((DoubleFloatStructImpl) number).value;
			return DoubleFloatStruct.toLispFloat(multiply);
		}
		return number.multiply(this);
	}

	@Override
	public NumberStruct divide(final NumberStruct number) {
		if (number instanceof FixnumStructImpl) {
			final float divide = value / ((FixnumStructImpl) number).value;
			return SingleFloatStruct.toLispFloat(divide);
		} else if (number instanceof LongnumStructImpl) {
			final float divide = value / ((LongnumStructImpl) number).value;
			return SingleFloatStruct.toLispFloat(divide);
		} else if (number instanceof BignumStructImpl) {
			final BigDecimal bigDecimal1 = BigDecimal.valueOf(value);
			final BigDecimal bigDecimal2 = new BigDecimal(((BignumStructImpl) number).value);
			final BigDecimal divide = bigDecimal1.divide(bigDecimal2, MathContext.DECIMAL32);
			return SingleFloatStruct.toLispFloat(divide.floatValue());
		} else if (number instanceof RatioStructImpl) {
			final float divide = value / ((RatioStructImpl) number).value.floatValue();
			return SingleFloatStruct.toLispFloat(divide);
		} else if (number instanceof SingleFloatStructImpl) {
			final float divide = value / ((SingleFloatStructImpl) number).value;
			return SingleFloatStruct.toLispFloat(divide);
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
			return Float.compare(value, ((SingleFloatStructImpl) number).value) == 0;
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
			return Float.compare(value, ((SingleFloatStructImpl) number).value) != 0;
		} else if (number instanceof DoubleFloatStructImpl) {
			return Double.compare(value, ((DoubleFloatStructImpl) number).value) != 0;
		} else if (number instanceof RationalStruct) {
			return rational().isNotEqualTo(number);
		}
		return number.isNotEqualTo(this);
	}

	@Override
	public FloatStruct signum() {
		final int compare = Float.compare(value, 0.0F);
		if (compare == 0) {
			return this;
		}
		if (compare > 0) {
			return SingleFloatStruct.ONE;
		}
		return SingleFloatStruct.MINUS_ONE;
	}

	@Override
	public FloatStruct negation() {
		return SingleFloatStruct.toLispFloat(-value);
	}

	@Override
	public FloatStruct reciprocal() {
		return SingleFloatStruct.toLispFloat(1 / value);
	}

	@Override
	public RealStruct exp() {
		final double exp = StrictMath.exp(value);
		return SingleFloatStruct.toLispFloat(exp);
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
		return SingleFloatStruct.toLispFloat(log);
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
		if (Float.compare(value, 0.0F) < 0) {
			final Apfloat ap = new Apfloat(value);
			final Apcomplex sqrt = ApcomplexMath.sqrt(ap);
			return ApfloatUtils.toNumberStruct(sqrt);
		}

		final double sqrt = StrictMath.sqrt(value);
		return SingleFloatStruct.toLispFloat(sqrt);
	}

	@Override
	public RealStruct sin() {
		final double sin = StrictMath.sin(value);
		return SingleFloatStruct.toLispFloat(sin);
	}

	@Override
	public RealStruct cos() {
		final double cos = StrictMath.cos(value);
		return SingleFloatStruct.toLispFloat(cos);
	}

	@Override
	public RealStruct tan() {
		final double tan = StrictMath.tan(value);
		return SingleFloatStruct.toLispFloat(tan);
	}

	@Override
	public RealStruct asin() {
		final double asin = StrictMath.asin(value);
		return SingleFloatStruct.toLispFloat(asin);
	}

	@Override
	public RealStruct acos() {
		final double acos = StrictMath.acos(value);
		return SingleFloatStruct.toLispFloat(acos);
	}

	@Override
	public RealStruct atan() {
		final double atan = StrictMath.atan(value);
		return SingleFloatStruct.toLispFloat(atan);
	}

	@Override
	public RealStruct sinh() {
		final double sinh = StrictMath.sinh(value);
		return SingleFloatStruct.toLispFloat(sinh);
	}

	@Override
	public RealStruct cosh() {
		final double cosh = StrictMath.cosh(value);
		return SingleFloatStruct.toLispFloat(cosh);
	}

	@Override
	public RealStruct tanh() {
		final double tanh = StrictMath.tanh(value);
		return SingleFloatStruct.toLispFloat(tanh);
	}

	@Override
	public RealStruct asinh() {
		final double asinh = FastMath.asinh(value);
		return SingleFloatStruct.toLispFloat(asinh);
	}

	@Override
	public RealStruct acosh() {
		final double acosh = FastMath.acosh(value);
		return SingleFloatStruct.toLispFloat(acosh);
	}

	@Override
	public RealStruct atanh() {
		final double atanh = FastMath.atanh(value);
		return SingleFloatStruct.toLispFloat(atanh);
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
