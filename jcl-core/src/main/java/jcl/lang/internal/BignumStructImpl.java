package jcl.lang.internal;

import java.math.BigInteger;
import java.math.RoundingMode;
import java.util.function.Function;

import com.google.common.math.BigIntegerMath;
import com.google.common.math.DoubleMath;
import jcl.lang.BignumStruct;
import jcl.lang.FloatStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.NumberStruct;
import jcl.lang.RationalStruct;
import jcl.lang.RealStruct;
import jcl.lang.SingleFloatStruct;
import jcl.lang.number.QuotientRemainder;
import jcl.type.BignumType;
import lombok.EqualsAndHashCode;
import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.util.ArithmeticUtils;
import org.apfloat.Apcomplex;
import org.apfloat.ApcomplexMath;
import org.apfloat.Apfloat;
import org.apfloat.ApfloatMath;
import org.apfloat.Apint;

/**
 * The {@link BignumStructImpl} is the object representation of a Lisp 'bignum' type where the value is not
 * representable by a Java {@literal long}.
 */
@EqualsAndHashCode(callSuper = false)
public final class BignumStructImpl extends IntegerStructImpl implements BignumStruct {

	/**
	 * The integer value.
	 */
	final BigInteger value;

	/**
	 * Public constructor.
	 *
	 * @param value
	 * 		the integer value
	 */
	public BignumStructImpl(final BigInteger value) {
		super(BignumType.INSTANCE);
		this.value = value;
	}

	@Override
	public IntegerStruct gcd(final IntegerStruct integer) {
		final BigInteger gcd = value.gcd(integer.toJavaBigInteger());
		return IntegerStruct.toLispInteger(gcd);
	}

	@Override
	public IntegerStruct lcm(final IntegerStruct integer) {
		final BigInteger lcm = lcm(value, integer.toJavaBigInteger());
		return IntegerStruct.toLispInteger(lcm);
	}

	@Override
	public IntegerStruct ash(final IntegerStruct count) {
		if (count.zerop()) {
			return this;
		}
		final int countInt = count.toJavaInt();

		// NOTE: shiftLeft will automatically take care of shiftRight based on the sign of countInt
		final BigInteger shiftedBigInteger = value.shiftLeft(countInt);
		return IntegerStruct.toLispInteger(shiftedBigInteger);
	}

	@Override
	public IntegerStruct logAnd(final IntegerStruct integer) {
		final BigInteger bigInteger = integer.toJavaBigInteger();
		return IntegerStruct.toLispInteger(value.and(bigInteger));
	}

	@Override
	public IntegerStruct logAndC1(final IntegerStruct integer) {
		final BigInteger bigInteger = integer.toJavaBigInteger();
		return IntegerStruct.toLispInteger(value.not().and(bigInteger));
	}

	@Override
	public IntegerStruct logAndC2(final IntegerStruct integer) {
		final BigInteger bigInteger = integer.toJavaBigInteger();
		return IntegerStruct.toLispInteger(value.and(bigInteger.not()));
	}

	@Override
	public IntegerStruct logEqv(final IntegerStruct integer) {
		final BigInteger bigInteger = integer.toJavaBigInteger();
		final BigInteger xor = value.xor(bigInteger);
		return IntegerStruct.toLispInteger(xor.not());
	}

	@Override
	public IntegerStruct logIor(final IntegerStruct integer) {
		final BigInteger bigInteger = integer.toJavaBigInteger();
		return IntegerStruct.toLispInteger(value.or(bigInteger));
	}

	@Override
	public IntegerStruct logNand(final IntegerStruct integer) {
		final BigInteger bigInteger = integer.toJavaBigInteger();
		final BigInteger and = value.and(bigInteger);
		return IntegerStruct.toLispInteger(and.not());
	}

	@Override
	public IntegerStruct logNor(final IntegerStruct integer) {
		final BigInteger bigInteger = integer.toJavaBigInteger();
		final BigInteger or = value.or(bigInteger);
		return IntegerStruct.toLispInteger(or.not());
	}

	@Override
	public IntegerStruct logNot() {
		return IntegerStruct.toLispInteger(value.not());
	}

	@Override
	public IntegerStruct logOrC1(final IntegerStruct integer) {
		final BigInteger bigInteger = integer.toJavaBigInteger();
		return IntegerStruct.toLispInteger(value.not().or(bigInteger));
	}

	@Override
	public IntegerStruct logOrC2(final IntegerStruct integer) {
		final BigInteger bigInteger = integer.toJavaBigInteger();
		return IntegerStruct.toLispInteger(value.or(bigInteger.not()));
	}

	@Override
	public IntegerStruct logXor(final IntegerStruct integer) {
		final BigInteger bigInteger = integer.toJavaBigInteger();
		return IntegerStruct.toLispInteger(value.xor(bigInteger));
	}

	@Override
	public boolean logBitP(final IntegerStruct index) {
		final int indexInt = index.toJavaInt();
		return value.testBit(indexInt);
	}

	@Override
	public IntegerStruct logCount() {
		final int bitCount = value.bitCount();
		return IntegerStruct.toLispInteger(bitCount);
	}

	@Override
	public boolean logTest(final IntegerStruct integer) {
		final BigInteger bigInteger = integer.toJavaBigInteger();
		final BigInteger and = value.and(bigInteger);
		return and.signum() != 0;
	}

	@Override
	public IntegerStruct integerLength() {
		final int bitLength = value.bitLength();
		return IntegerStruct.toLispInteger(bitLength);
	}

	@Override
	public boolean evenp() {
		return !value.testBit(0);
	}

	@Override
	public boolean oddp() {
		return value.testBit(0);
	}

	@Override
	public NumberStruct isqrt() {
		if (value.signum() < 0) {
			final Apint ap = new Apint(value);
			final Apcomplex sqrt = ApcomplexMath.sqrt(ap);
			return ApfloatUtils.toNumberStruct(sqrt);
		}

		final BigInteger isqrt = BigIntegerMath.sqrt(value, RoundingMode.FLOOR);
		return IntegerStruct.toLispInteger(isqrt);
	}

	@Override
	public int toJavaInt() {
		return value.intValue();
	}

	@Override
	public Integer toJavaInteger() {
		return value.intValue();
	}

	@Override
	public long toJavaPLong() {
		return value.longValue();
	}

	@Override
	public Long toJavaLong() {
		return value.longValue();
	}

	@Override
	public BigInteger toJavaBigInteger() {
		return value;
	}

	/*
	RATIONAL-STRUCT
	 */

	@Override
	public BigFraction toJavaBigFraction() {
		return new BigFraction(value);
	}

	/*
	REAL-STRUCT
	 */

	@Override
	public boolean isLessThan(final RealStruct real) {
		if (real instanceof IntegerStruct) {
			final BigInteger bigInteger = ((IntegerStruct) real).toJavaBigInteger();
			return value.compareTo(bigInteger) < 0;
		} else if (real instanceof RatioStructImpl) {
			final BigFraction bigFraction1 = new BigFraction(value);
			final BigFraction bigFraction2 = ((RatioStructImpl) real).value;
			return bigFraction1.compareTo(bigFraction2) < 0;
		}
		return isLessThan(real.rational());
	}

	@Override
	public boolean isGreaterThan(final RealStruct real) {
		if (real instanceof IntegerStruct) {
			final BigInteger bigInteger = ((IntegerStruct) real).toJavaBigInteger();
			return value.compareTo(bigInteger) > 0;
		} else if (real instanceof RatioStructImpl) {
			final BigFraction bigFraction1 = new BigFraction(value);
			final BigFraction bigFraction2 = ((RatioStructImpl) real).value;
			return bigFraction1.compareTo(bigFraction2) > 0;
		}
		return isGreaterThan(real.rational());
	}

	@Override
	public boolean isLessThanOrEqualTo(final RealStruct real) {
		if (real instanceof IntegerStruct) {
			final BigInteger bigInteger = ((IntegerStruct) real).toJavaBigInteger();
			return value.compareTo(bigInteger) <= 0;
		} else if (real instanceof RatioStructImpl) {
			final BigFraction bigFraction1 = new BigFraction(value);
			final BigFraction bigFraction2 = ((RatioStructImpl) real).value;
			return bigFraction1.compareTo(bigFraction2) <= 0;
		}
		return isLessThanOrEqualTo(real.rational());
	}

	@Override
	public boolean isGreaterThanOrEqualTo(final RealStruct real) {
		if (real instanceof IntegerStruct) {
			final BigInteger bigInteger = ((IntegerStruct) real).toJavaBigInteger();
			return value.compareTo(bigInteger) >= 0;
		} else if (real instanceof RatioStructImpl) {
			final BigFraction bigFraction1 = new BigFraction(value);
			final BigFraction bigFraction2 = ((RatioStructImpl) real).value;
			return bigFraction1.compareTo(bigFraction2) >= 0;
		}
		return isGreaterThanOrEqualTo(real.rational());
	}

	@Override
	public boolean plusp() {
		return value.signum() > 0;
	}

	@Override
	public boolean minusp() {
		return value.signum() < 0;
	}

	@Override
	public FloatStruct floatingPoint() {
		return new SingleFloatStructImpl(value.floatValue());
	}

	@Override
	public FloatStruct floatingPoint(final FloatStruct prototype) {
		if (prototype instanceof SingleFloatStruct) {
			return new SingleFloatStructImpl(value.floatValue());
		} else {
			return new DoubleFloatStructImpl(value.doubleValue());
		}
	}

	@Override
	public RealStruct mod(final RealStruct divisor) {
		// TODO
		final QuotientRemainder floor = floor(divisor);
		return floor.getRemainder();
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
			final BigInteger divisorValue = BigInteger.valueOf(((FixnumStructImpl) divisor).value);
			final BigInteger[] results = value.divideAndRemainder(divisorValue);
			final BigInteger quotient = results[0];
			final BigInteger remainder = results[1];

			return new QuotientRemainder(
					IntegerStruct.toLispInteger(quotient),
					IntegerStruct.toLispInteger(remainder)
			);
		} else if (divisor instanceof LongnumStructImpl) {
			final BigInteger divisorValue = BigInteger.valueOf(((LongnumStructImpl) divisor).value);
			final BigInteger[] results = value.divideAndRemainder(divisorValue);
			final BigInteger quotient = results[0];
			final BigInteger remainder = results[1];

			return new QuotientRemainder(
					IntegerStruct.toLispInteger(quotient),
					IntegerStruct.toLispInteger(remainder)
			);
		} else if (divisor instanceof BignumStructImpl) {
			final BigInteger divisorValue = ((BignumStructImpl) divisor).value;
			final BigInteger[] results = value.divideAndRemainder(divisorValue);
			final BigInteger quotient = results[0];
			final BigInteger remainder = results[1];

			return new QuotientRemainder(
					IntegerStruct.toLispInteger(quotient),
					IntegerStruct.toLispInteger(remainder)
			);
		} else if (divisor instanceof RatioStructImpl) {
			final BigFraction divisorValue = ((RatioStructImpl) divisor).value;
			final BigInteger numerator = divisorValue.getNumerator();
			final BigInteger denominator = divisorValue.getDenominator();

			final BigInteger quotient = value.multiply(denominator).divide(numerator);

			final BigFraction valBF = new BigFraction(value);
			final BigFraction quotientBF = new BigFraction(quotient);
			final BigFraction remainder = valBF.subtract(quotientBF.multiply(divisorValue));

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
		return new QuotientRemainder(this, ZERO);
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
		return new QuotientRemainder(new SingleFloatStructImpl(value.floatValue()), ZERO);
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
		return new QuotientRemainder(this, ZERO);
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
		return new QuotientRemainder(new SingleFloatStructImpl(value.floatValue()), ZERO);
	}

	@SuppressWarnings("NumericCastThatLosesPrecision")
	@Override
	public QuotientRemainder fceiling(final RealStruct divisor) {
		return calculateQuotientRemainder(divisor, val -> (long) StrictMath.ceil(val),
		                                  toLispFloat(divisor));
	}

	@Override
	public QuotientRemainder truncate() {
		return new QuotientRemainder(this, ZERO);
	}

	@Override
	public QuotientRemainder truncate(final RealStruct divisor) {
		return calculateQuotientRemainder(divisor,
		                                  val -> DoubleMath.roundToLong(value.doubleValue(), RoundingMode.DOWN),
		                                  IntegerStruct::toLispInteger);
	}

	@Override
	public QuotientRemainder ftruncate() {
		return new QuotientRemainder(new SingleFloatStructImpl(value.floatValue()), ZERO);
	}

	@Override
	public QuotientRemainder ftruncate(final RealStruct divisor) {
		return calculateQuotientRemainder(divisor,
		                                  val -> DoubleMath.roundToLong(value.doubleValue(), RoundingMode.DOWN),
		                                  toLispFloat(divisor));
	}

	@Override
	public QuotientRemainder round() {
		return new QuotientRemainder(this, ZERO);
	}

	@Override
	public QuotientRemainder round(final RealStruct divisor) {
		return calculateQuotientRemainder(divisor,
		                                  val -> DoubleMath.roundToLong(value.doubleValue(), RoundingMode.HALF_EVEN),
		                                  IntegerStruct::toLispInteger);
	}

	@Override
	public QuotientRemainder fround() {
		return new QuotientRemainder(new SingleFloatStructImpl(value.floatValue()), ZERO);
	}

	@Override
	public QuotientRemainder fround(final RealStruct divisor) {
		return calculateQuotientRemainder(divisor,
		                                  val -> DoubleMath.roundToLong(value.doubleValue(), RoundingMode.HALF_EVEN),
		                                  toLispFloat(divisor));
	}

	@Override
	public RealStruct atan(final RealStruct real) {
		final Apint ap = new Apint(value);
		final Apfloat realAp = real.ap();
		final Apfloat atan2 = ApfloatMath.atan2(ap, realAp);
		return new SingleFloatStructImpl(atan2.floatValue());
	}

	/*
	NUMBER-STRUCT
	 */


	@Override
	public Apint ap() {
		return new Apint(value);
	}

	@Override
	public IntegerStruct abs() {
		return new BignumStructImpl(value.abs());
	}

	@Override
	public boolean zerop() {
		return value.signum() == 0;
	}

	@Override
	public NumberStruct add(final NumberStruct number) {
		if (number instanceof IntegerStruct) {
			final BigInteger add = value.add(((IntegerStruct) number).toJavaBigInteger());
			return new BignumStructImpl(add);
		} else if (number instanceof RatioStructImpl) {
			final BigFraction add = ((RatioStructImpl) number).value.add(value);
			return RationalStruct.toLispRational(add);
		} else if (number instanceof SingleFloatStructImpl) {
			final float f = value.floatValue();
			final float add = f + ((SingleFloatStructImpl) number).value;
			return new SingleFloatStructImpl(add);
		} else if (number instanceof DoubleFloatStructImpl) {
			final double d = value.doubleValue();
			final double add = d + ((DoubleFloatStructImpl) number).value;
			return new DoubleFloatStructImpl(add);
		}
		final Apint ap = new Apint(value);
		final Apcomplex numberAp = number.ap();
		final Apcomplex add = ap.add(numberAp);
		return ApfloatUtils.toNumberStruct(add);
	}

	@Override
	public NumberStruct subtract(final NumberStruct number) {
		if (number instanceof IntegerStruct) {
			final BigInteger subtract = value.subtract(((IntegerStruct) number).toJavaBigInteger());
			return new BignumStructImpl(subtract);
		} else if (number instanceof RatioStructImpl) {
			final BigFraction subtract = new BigFraction(value).subtract(((RatioStructImpl) number).value);
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
		final Apint ap = new Apint(value);
		final Apcomplex numberAp = number.ap();
		final Apcomplex subtract = ap.subtract(numberAp);
		return ApfloatUtils.toNumberStruct(subtract);
	}

	@Override
	public NumberStruct multiply(final NumberStruct number) {
		if (number instanceof IntegerStruct) {
			final BigInteger multiply = value.multiply(((IntegerStruct) number).toJavaBigInteger());
			return new BignumStructImpl(multiply);
		} else if (number instanceof RatioStructImpl) {
			final BigFraction multiply = ((RatioStructImpl) number).value.multiply(value);
			return RationalStruct.toLispRational(multiply);
		} else if (number instanceof SingleFloatStructImpl) {
			final float f = value.floatValue();
			final float multiply = f * ((SingleFloatStructImpl) number).value;
			return new SingleFloatStructImpl(multiply);
		} else if (number instanceof DoubleFloatStructImpl) {
			final double d = value.doubleValue();
			final double multiply = d * ((DoubleFloatStructImpl) number).value;
			return new DoubleFloatStructImpl(multiply);
		}
		final Apint ap = new Apint(value);
		final Apcomplex numberAp = number.ap();
		final Apcomplex multiply = ap.multiply(numberAp);
		return ApfloatUtils.toNumberStruct(multiply);
	}

	@Override
	public NumberStruct divide(final NumberStruct number) {
		if (number instanceof IntegerStruct) {
			return RationalStruct.toLispRational(this, (IntegerStruct) number);
		} else if (number instanceof RatioStructImpl) {
			final BigFraction divide = new BigFraction(value).divide(((RatioStructImpl) number).value);
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
		final Apint ap = new Apint(value);
		final Apcomplex numberAp = number.ap();
		final Apcomplex divide = ap.divide(numberAp);
		return ApfloatUtils.toNumberStruct(divide);
	}

	@Override
	public boolean isEqualTo(final NumberStruct number) {
		if (number instanceof IntegerStruct) {
			final BigInteger bigInteger = ((IntegerStruct) number).toJavaBigInteger();
			return value.compareTo(bigInteger) == 0;
		}
		return number.isEqualTo(this);
	}

	@Override
	public boolean isNotEqualTo(final NumberStruct number) {
		if (number instanceof IntegerStruct) {
			final BigInteger bigInteger = ((IntegerStruct) number).toJavaBigInteger();
			return value.compareTo(bigInteger) != 0;
		}
		return number.isNotEqualTo(this);
	}

	@Override
	public IntegerStruct signum() {
		final int signum = value.signum();
		if (signum == 0) {
			return this;
		}
		if (signum > 0) {
			return ONE;
		}
		return MINUS_ONE;
	}

	@Override
	public IntegerStruct negation() {
		return new BignumStructImpl(value.negate());
	}

	@Override
	public RationalStruct reciprocal() {
		if (BigInteger.ONE.compareTo(value) == 0) {
			return this;
		}
		return new RatioStructImpl(ONE, this);
	}

	@Override
	public RealStruct exp() {
		final Apint ap = new Apint(value);
		final Apfloat exp = ApfloatMath.exp(ap);
		return new SingleFloatStructImpl(exp.floatValue());
	}

	@Override
	public NumberStruct expt(final NumberStruct power) {
		if (power instanceof IntegerStruct) {
			final BigInteger bigInteger2 = ((IntegerStruct) power).toJavaBigInteger();
			final BigInteger pow = ArithmeticUtils.pow(value, bigInteger2);
			return IntegerStruct.toLispInteger(pow);
		}
		final Apint ap = new Apint(value);
		final Apcomplex powerAp = power.ap();
		final Apcomplex pow = ApcomplexMath.pow(ap, powerAp);
		return ApfloatUtils.toNumberStruct(pow);
	}

	@Override
	public RealStruct log() {
		final Apint ap = new Apint(value);
		final Apfloat log = ApfloatMath.log(ap);
		return new SingleFloatStructImpl(log.floatValue());
	}

	@Override
	public NumberStruct log(final NumberStruct base) {
		final Apint ap = new Apint(value);
		final Apcomplex baseAp = base.ap();
		final Apcomplex log = ApcomplexMath.log(ap, baseAp);
		return ApfloatUtils.toNumberStruct(log);
	}

	@Override
	public NumberStruct sqrt() {
		if (value.signum() < 0) {
			final Apint ap = new Apint(value);
			final Apcomplex sqrt = ApcomplexMath.sqrt(ap);
			return ApfloatUtils.toNumberStruct(sqrt);
		}

		final Apint ap = new Apint(value);
		final Apfloat sqrt = ApfloatMath.sqrt(ap);
		return new SingleFloatStructImpl(sqrt.floatValue());
	}

	@Override
	public RealStruct sin() {
		final Apint ap = new Apint(value);
		final Apfloat sin = ApfloatMath.sin(ap);
		return new SingleFloatStructImpl(sin.floatValue());
	}

	@Override
	public RealStruct cos() {
		final Apint ap = new Apint(value);
		final Apfloat cos = ApfloatMath.cos(ap);
		return new SingleFloatStructImpl(cos.floatValue());
	}

	@Override
	public RealStruct tan() {
		final Apint ap = new Apint(value);
		final Apfloat tan = ApfloatMath.tan(ap);
		return new SingleFloatStructImpl(tan.floatValue());
	}

	@Override
	public RealStruct asin() {
		final Apint ap = new Apint(value);
		final Apfloat asin = ApfloatMath.asin(ap);
		return new SingleFloatStructImpl(asin.floatValue());
	}

	@Override
	public RealStruct acos() {
		final Apint ap = new Apint(value);
		final Apfloat acos = ApfloatMath.acos(ap);
		return new SingleFloatStructImpl(acos.floatValue());
	}

	@Override
	public RealStruct atan() {
		final Apint ap = new Apint(value);
		final Apfloat atan = ApfloatMath.atan(ap);
		return new SingleFloatStructImpl(atan.floatValue());
	}

	@Override
	public RealStruct sinh() {
		final Apint ap = new Apint(value);
		final Apfloat sinh = ApfloatMath.sinh(ap);
		return new SingleFloatStructImpl(sinh.floatValue());
	}

	@Override
	public RealStruct cosh() {
		final Apint ap = new Apint(value);
		final Apfloat cosh = ApfloatMath.cosh(ap);
		return new SingleFloatStructImpl(cosh.floatValue());
	}

	@Override
	public RealStruct tanh() {
		final Apint ap = new Apint(value);
		final Apfloat tanh = ApfloatMath.tanh(ap);
		return new SingleFloatStructImpl(tanh.floatValue());
	}

	@Override
	public RealStruct asinh() {
		final Apint ap = new Apint(value);
		final Apfloat asinh = ApfloatMath.asinh(ap);
		return new SingleFloatStructImpl(asinh.floatValue());
	}

	@Override
	public RealStruct acosh() {
		final Apint ap = new Apint(value);
		final Apfloat acosh = ApfloatMath.acosh(ap);
		return new SingleFloatStructImpl(acosh.floatValue());
	}

	@Override
	public RealStruct atanh() {
		final Apint ap = new Apint(value);
		final Apfloat atanh = ApfloatMath.atanh(ap);
		return new SingleFloatStructImpl(atanh.floatValue());
	}
}
