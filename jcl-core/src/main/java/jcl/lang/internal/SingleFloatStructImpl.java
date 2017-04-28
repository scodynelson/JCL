package jcl.lang.internal;

import java.math.BigDecimal;
import java.math.BigInteger;

import jcl.lang.DoubleFloatStruct;
import jcl.lang.FloatStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.NumberStruct;
import jcl.lang.RationalStruct;
import jcl.lang.RealStruct;
import jcl.lang.ShortFloatStruct;
import jcl.lang.SingleFloatStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.internal.number.FloatStructImpl;
import jcl.lang.internal.number.IntegerStructImpl;
import jcl.lang.internal.number.RatioStructImpl;
import jcl.lang.number.DecodeFloatResult;
import jcl.lang.number.QuotientRemainder;
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
import org.apfloat.Apint;
import org.apfloat.FixedPrecisionApfloatHelper;

@EqualsAndHashCode(callSuper = true)
public class SingleFloatStructImpl extends BuiltInClassStruct implements SingleFloatStruct, ShortFloatStruct {

	/**
	 * The floating-point precision of a FloatStruct object.
	 */
	private static final int FLOAT_PRECISION = 24;

	private final float value;

	private static final FixedPrecisionApfloatHelper APFLOAT_HELPER = new FixedPrecisionApfloatHelper(FLOAT_PRECISION);

	public SingleFloatStructImpl(final float value) {
		super(SingleFloatType.INSTANCE, null, null);
		this.value = value;
	}

	@Override
	public float floatValue() {
		return value;
	}

	@Override
	public double doubleValue() {
		return value;
	}

	@Override
	public BigDecimal bigDecimalValue() {
		return BigDecimal.valueOf(value);
	}

	@Override
	public DecodeFloatResult decodeFloat() {
		final int bits = Float.floatToRawIntBits(value);
		final DecodedFloat decodedFloat = getDecodedFloat(bits);

		final int mantissa = decodedFloat.getMantissa();
		final int expt = ArithmeticUtils.pow(2, FLOAT_PRECISION);
		final int significand = mantissa / expt;
		final SingleFloatStruct significandFloat = SingleFloatStruct.toLispFloat((float) significand);

		final int storedExponent = decodedFloat.getStoredExponent();
		// 127 + 23 = 150
		final int exponent = (storedExponent - 150) + FLOAT_PRECISION;
		final IntegerStruct exponentInteger = IntegerStruct.toLispInteger(exponent);

		final int sign = decodedFloat.getSign();
		// TODO: SingleFloatStruct
		final FloatStruct signFloat = (sign == 1) ? FloatStruct.ONE : FloatStruct.MINUS_ONE;

		return new DecodeFloatResult(significandFloat, exponentInteger, signFloat);
	}

	@Override
	public DecodeFloatResult integerDecodeFloat() {
		final int bits = Float.floatToRawIntBits(value);
		final DecodedFloat decodedFloat = getDecodedFloat(bits);

		final int mantissa = decodedFloat.getMantissa();
		final IntegerStruct significandInteger = IntegerStructImpl.valueOf(mantissa);

		final int storedExponent = decodedFloat.getStoredExponent();
		// 127 + 23 = 150
		final int exponent = storedExponent - 150;
		final IntegerStruct exponentInteger = IntegerStructImpl.valueOf(exponent);

		final int sign = decodedFloat.getSign();
		final IntegerStruct signInteger = (sign == 1) ? IntegerStruct.ONE : IntegerStruct.MINUS_ONE;

		return new DecodeFloatResult(significandInteger, exponentInteger, signInteger);
	}

	@Override
	public IntegerStruct floatPrecision() {
		return IntegerStruct.toLispInteger(FLOAT_PRECISION);
	}

	@Override
	public FloatStruct floatSign() {
		final int bits = Float.floatToRawIntBits(value);
		return (bits < 0) ? FloatStruct.MINUS_ONE : FloatStruct.ONE;
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
			return value < ((SingleFloatStructImpl) real).floatValue();
		} else if (real instanceof DoubleFloatStructImpl) {
			return value < ((DoubleFloatStructImpl) real).doubleValue();
		}
		return rational().isLessThan(real);
	}

	@Override
	public boolean isGreaterThan(final RealStruct real) {
		if (real instanceof SingleFloatStructImpl) {
			return value > ((SingleFloatStructImpl) real).floatValue();
		} else if (real instanceof DoubleFloatStructImpl) {
			return value > ((DoubleFloatStructImpl) real).doubleValue();
		}
		return rational().isLessThan(real);
	}

	@Override
	public boolean isLessThanOrEqualTo(final RealStruct real) {
		if (real instanceof SingleFloatStructImpl) {
			return value <= ((SingleFloatStructImpl) real).floatValue();
		} else if (real instanceof DoubleFloatStructImpl) {
			return value <= ((DoubleFloatStructImpl) real).doubleValue();
		}
		return rational().isLessThan(real);
	}

	@Override
	public boolean isGreaterThanOrEqualTo(final RealStruct real) {
		if (real instanceof SingleFloatStructImpl) {
			return value >= ((SingleFloatStructImpl) real).floatValue();
		} else if (real instanceof DoubleFloatStructImpl) {
			return value >= ((DoubleFloatStructImpl) real).doubleValue();
		}
		return rational().isLessThan(real);
	}

	@Override
	public boolean plusp() {
		return value > 0;
	}

	@Override
	public boolean minusp() {
		return value < 0;
	}

	@Override
	public RationalStruct rational() {
		final BigFraction bigFraction = new BigFraction(value);
		final BigFraction bigFractionReduced = bigFraction.reduce();

		final BigInteger numerator = bigFractionReduced.getNumerator();
		final BigInteger denominator = bigFractionReduced.getDenominator();

		if (BigInteger.ONE.equals(denominator)) {
			return IntegerStructImpl.valueOf(numerator);
		}

		final Apint numeratorAp = new Apint(numerator);
		final Apint denominatorAp = new Apint(denominator);
		return RatioStructImpl.valueOf(numeratorAp, denominatorAp);
	}

	@Override
	public FloatStruct floatingPoint(final FloatStruct prototype) {
		if (prototype instanceof SingleFloatStructImpl) {
			return floatingPoint();
		} else {
			return DoubleFloatStruct.toLispFloat((double) value);
		}
	}

	@Override
	public QuotientRemainder floor() {
		// TODO
		return null;
	}

	@Override
	public QuotientRemainder floor(final RealStruct divisor) {
		// TODO
		return null;
	}

	@Override
	public QuotientRemainder ffloor() {
		// TODO
		return null;
	}

	@Override
	public QuotientRemainder ffloor(final RealStruct divisor) {
		// TODO
		return null;
	}

	@Override
	public QuotientRemainder ceiling() {
		// TODO
		return null;
	}

	@Override
	public QuotientRemainder ceiling(final RealStruct divisor) {
		// TODO
		return null;
	}

	@Override
	public QuotientRemainder fceiling() {
		// TODO
		return null;
	}

	@Override
	public QuotientRemainder fceiling(final RealStruct divisor) {
		// TODO
		return null;
	}

	@Override
	public QuotientRemainder truncate() {
		// TODO
		return null;
	}

	@Override
	public QuotientRemainder truncate(final RealStruct divisor) {
		// TODO
		return null;
	}

	@Override
	public QuotientRemainder ftruncate() {
		// TODO
		return null;
	}

	@Override
	public QuotientRemainder ftruncate(final RealStruct divisor) {
		// TODO
		return null;
	}

	@Override
	public QuotientRemainder round() {
		// TODO
		return null;
	}

	@Override
	public QuotientRemainder round(final RealStruct divisor) {
		// TODO
		return null;
	}

	@Override
	public QuotientRemainder fround() {
		// TODO
		return null;
	}

	@Override
	public QuotientRemainder fround(final RealStruct divisor) {
		// TODO
		return null;
	}

	@Override
	public RealStruct atan(final RealStruct real) {
		if (real instanceof FixnumStructImpl) {
			final double atan2 = StrictMath.atan2(value, ((FixnumStructImpl) real).intValue());
			return DoubleFloatStruct.toLispFloat(atan2);
		} else if (real instanceof LongnumStructImpl) {
			final double atan2 = StrictMath.atan2(value, ((LongnumStructImpl) real).longValue());
			return DoubleFloatStruct.toLispFloat(atan2);
		} else if (real instanceof SingleFloatStructImpl) {
			final double atan2 = StrictMath.atan2(value, ((SingleFloatStructImpl) real).floatValue());
			return DoubleFloatStruct.toLispFloat(atan2);
		} else if (real instanceof DoubleFloatStructImpl) {
			final double atan2 = StrictMath.atan2(value, ((DoubleFloatStructImpl) real).doubleValue());
			return DoubleFloatStruct.toLispFloat(atan2);
		}
		final Apfloat ap = new Apfloat(value);
		final Apfloat realAp = real.ap();
		final Apfloat atan2 = ApfloatMath.atan2(ap, realAp);
		return DoubleFloatStruct.toLispFloat(atan2.doubleValue());
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
		return value == 0;
	}

	@Override
	public NumberStruct add(final NumberStruct number) {
		// TODO
		return null;
	}

	@Override
	public NumberStruct subtract(final NumberStruct number) {
		// TODO
		return null;
	}

	@Override
	public NumberStruct multiply(final NumberStruct number) {
		// TODO
		return null;
	}

	@Override
	public NumberStruct divide(final NumberStruct number) {
		// TODO
		return null;
	}

	@Override
	public boolean isEqualTo(final NumberStruct number) {
		if (number instanceof SingleFloatStructImpl) {
			return Float.compare(value, ((SingleFloatStructImpl) number).floatValue()) == 0;
		} else if (number instanceof DoubleFloatStructImpl) {
			return Double.compare(value, ((DoubleFloatStructImpl) number).doubleValue()) == 0;
		} else if (number instanceof RationalStruct) {
			return rational().isEqualTo(number);
		}
		return number.isEqualTo(this);
	}

	@Override
	public boolean isNotEqualTo(final NumberStruct number) {
		if (number instanceof SingleFloatStructImpl) {
			return Float.compare(value, ((SingleFloatStructImpl) number).floatValue()) != 0;
		} else if (number instanceof DoubleFloatStructImpl) {
			return Double.compare(value, ((DoubleFloatStructImpl) number).doubleValue()) != 0;
		} else if (number instanceof RationalStruct) {
			return rational().isNotEqualTo(number);
		}
		return number.isNotEqualTo(this);
	}

	@Override
	public FloatStruct signum() {
		if (value == 0) {
			return this;
		}
		if (value > 0) {
			return FloatStruct.ONE;
		}
		return FloatStruct.MINUS_ONE;
	}

	@Override
	public FloatStruct negation() {
		return null;
	}

	@Override
	public FloatStruct reciprocal() {
		// TODO
		return null;
	}

	@Override
	public RealStruct exp() {
		final double exp = StrictMath.exp(value);
		return FloatStructImpl.valueOf(exp);
	}

	@Override
	public NumberStruct expt(final NumberStruct power) {
		// TODO
		final Apfloat ap = new Apfloat(value);
		final Apcomplex powerAp = power.ap();
		final Apcomplex pow = ApcomplexMath.pow(ap, powerAp);
		return NumberStruct.valueOf(pow);
	}

	@Override
	public RealStruct log() {
		final double log = StrictMath.log(value);
		return FloatStructImpl.valueOf(log);
	}

	@Override
	public NumberStruct log(final NumberStruct base) {
		// TODO
		final Apfloat ap = new Apfloat(value);
		final Apcomplex baseAp = base.ap();
		final Apcomplex log = ApcomplexMath.log(ap, baseAp);
		return NumberStruct.valueOf(log);
	}

	@Override
	public RealStruct sqrt() {
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
}
