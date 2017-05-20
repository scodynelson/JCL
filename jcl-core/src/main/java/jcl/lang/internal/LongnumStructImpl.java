package jcl.lang.internal;

import java.math.BigInteger;
import java.math.RoundingMode;
import java.util.function.Function;

import com.google.common.math.DoubleMath;
import com.google.common.math.LongMath;
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
import org.apache.commons.math3.util.FastMath;
import org.apfloat.Apcomplex;
import org.apfloat.ApcomplexMath;
import org.apfloat.Apfloat;
import org.apfloat.ApfloatMath;
import org.apfloat.Apint;

/**
 * The {@link LongnumStructImpl} is the object representation of a Lisp 'bignum' type where the value is representable
 * by a Java {@literal long}.
 */
@EqualsAndHashCode(callSuper = false)
public final class LongnumStructImpl extends IntegerStructImpl implements BignumStruct {

	/**
	 * The integer value.
	 */
	final long value;

	/**
	 * Public constructor.
	 *
	 * @param value
	 * 		the integer value
	 */
	public LongnumStructImpl(final long value) {
		super(BignumType.INSTANCE);
		this.value = value;
	}

	@Override
	public IntegerStruct gcd(final IntegerStruct integer) {
		if (integer instanceof FixnumStructImpl) {
			final long gcd = ArithmeticUtils.gcd(value, ((FixnumStructImpl) integer).value);
			return IntegerStruct.toLispInteger(gcd);
		} else if (integer instanceof LongnumStructImpl) {
			final long gcd = ArithmeticUtils.gcd(value, ((LongnumStructImpl) integer).value);
			return IntegerStruct.toLispInteger(gcd);
		} else {
			final BigInteger bigInteger1 = BigInteger.valueOf(value);
			final BigInteger bigInteger2 = ((BignumStructImpl) integer).value;
			final BigInteger gcd = bigInteger1.gcd(bigInteger2);
			return IntegerStruct.toLispInteger(gcd);
		}
	}

	@Override
	public IntegerStruct lcm(final IntegerStruct integer) {
		if (integer instanceof FixnumStructImpl) {
			final long lcm = ArithmeticUtils.lcm(value, ((FixnumStructImpl) integer).value);
			return IntegerStruct.toLispInteger(lcm);
		} else if (integer instanceof LongnumStructImpl) {
			final long lcm = ArithmeticUtils.lcm(value, ((LongnumStructImpl) integer).value);
			return IntegerStruct.toLispInteger(lcm);
		} else {
			final BigInteger bigInteger1 = BigInteger.valueOf(value);
			final BigInteger bigInteger2 = integer.toJavaBigInteger();
			final BigInteger lcm = lcm(bigInteger1, bigInteger2);
			return IntegerStruct.toLispInteger(lcm);
		}
	}

	@Override
	public IntegerStruct ash(final IntegerStruct count) {
		if (count.zerop()) {
			return this;
		}
		final int countInt = count.toJavaInt();

		// NOTE: shiftLeft will automatically take care of shiftRight based on the sign of countInt
		final BigInteger bigInteger = BigInteger.valueOf(value);
		final BigInteger shiftedBigInteger = bigInteger.shiftLeft(countInt);
		return IntegerStruct.toLispInteger(shiftedBigInteger);
	}

	@Override
	public IntegerStruct logAnd(final IntegerStruct integer) {
		if (integer instanceof FixnumStructImpl) {
			return IntegerStruct.toLispInteger(value & ((FixnumStructImpl) integer).value);
		} else if (integer instanceof LongnumStructImpl) {
			return IntegerStruct.toLispInteger(value & ((LongnumStructImpl) integer).value);
		} else {
			final BigInteger bigInteger1 = BigInteger.valueOf(value);
			final BigInteger bigInteger2 = ((BignumStructImpl) integer).value;
			return IntegerStruct.toLispInteger(bigInteger1.and(bigInteger2));
		}
	}

	@Override
	public IntegerStruct logAndC1(final IntegerStruct integer) {
		if (integer instanceof FixnumStructImpl) {
			return IntegerStruct.toLispInteger(~value & ((FixnumStructImpl) integer).value);
		} else if (integer instanceof LongnumStructImpl) {
			return IntegerStruct.toLispInteger(~value & ((LongnumStructImpl) integer).value);
		} else {
			final BigInteger bigInteger1 = BigInteger.valueOf(value);
			final BigInteger bigInteger2 = ((BignumStructImpl) integer).value;
			return IntegerStruct.toLispInteger(bigInteger1.not().and(bigInteger2));
		}
	}

	@Override
	public IntegerStruct logAndC2(final IntegerStruct integer) {
		if (integer instanceof FixnumStructImpl) {
			return IntegerStruct.toLispInteger(value & ~((FixnumStructImpl) integer).value);
		} else if (integer instanceof LongnumStructImpl) {
			return IntegerStruct.toLispInteger(value & ~((LongnumStructImpl) integer).value);
		} else {
			final BigInteger bigInteger1 = BigInteger.valueOf(value);
			final BigInteger bigInteger2 = ((BignumStructImpl) integer).value;
			return IntegerStruct.toLispInteger(bigInteger1.and(bigInteger2.not()));
		}
	}

	@Override
	public IntegerStruct logEqv(final IntegerStruct integer) {
		if (integer instanceof FixnumStructImpl) {
			return IntegerStruct.toLispInteger(~(value ^ ((FixnumStructImpl) integer).value));
		} else if (integer instanceof LongnumStructImpl) {
			return IntegerStruct.toLispInteger(~(value ^ ((LongnumStructImpl) integer).value));
		} else {
			final BigInteger bigInteger1 = BigInteger.valueOf(value);
			final BigInteger bigInteger2 = ((BignumStructImpl) integer).value;
			final BigInteger xor = bigInteger1.xor(bigInteger2);
			return IntegerStruct.toLispInteger(xor.not());
		}
	}

	@Override
	public IntegerStruct logIor(final IntegerStruct integer) {
		if (integer instanceof FixnumStructImpl) {
			return IntegerStruct.toLispInteger(value | ((FixnumStructImpl) integer).value);
		} else if (integer instanceof LongnumStructImpl) {
			return IntegerStruct.toLispInteger(value | ((LongnumStructImpl) integer).value);
		} else {
			final BigInteger bigInteger1 = BigInteger.valueOf(value);
			final BigInteger bigInteger2 = ((BignumStructImpl) integer).value;
			return IntegerStruct.toLispInteger(bigInteger1.or(bigInteger2));
		}
	}

	@Override
	public IntegerStruct logNand(final IntegerStruct integer) {
		if (integer instanceof FixnumStructImpl) {
			return IntegerStruct.toLispInteger(~(value & ((FixnumStructImpl) integer).value));
		} else if (integer instanceof LongnumStructImpl) {
			return IntegerStruct.toLispInteger(~(value & ((LongnumStructImpl) integer).value));
		} else {
			final BigInteger bigInteger1 = BigInteger.valueOf(value);
			final BigInteger bigInteger2 = ((BignumStructImpl) integer).value;
			final BigInteger and = bigInteger1.and(bigInteger2);
			return IntegerStruct.toLispInteger(and.not());
		}
	}

	@Override
	public IntegerStruct logNor(final IntegerStruct integer) {
		if (integer instanceof FixnumStructImpl) {
			return IntegerStruct.toLispInteger(~(value | ((FixnumStructImpl) integer).value));
		} else if (integer instanceof LongnumStructImpl) {
			return IntegerStruct.toLispInteger(~(value | ((LongnumStructImpl) integer).value));
		} else {
			final BigInteger bigInteger1 = BigInteger.valueOf(value);
			final BigInteger bigInteger2 = ((BignumStructImpl) integer).value;
			final BigInteger or = bigInteger1.or(bigInteger2);
			return IntegerStruct.toLispInteger(or.not());
		}
	}

	@Override
	public IntegerStruct logNot() {
		return IntegerStruct.toLispInteger(~value);
	}

	@Override
	public IntegerStruct logOrC1(final IntegerStruct integer) {
		if (integer instanceof FixnumStructImpl) {
			return IntegerStruct.toLispInteger(~value | ((FixnumStructImpl) integer).value);
		} else if (integer instanceof LongnumStructImpl) {
			return IntegerStruct.toLispInteger(~value | ((LongnumStructImpl) integer).value);
		} else {
			final BigInteger bigInteger1 = BigInteger.valueOf(value);
			final BigInteger bigInteger2 = ((BignumStructImpl) integer).value;
			return IntegerStruct.toLispInteger(bigInteger1.not().or(bigInteger2));
		}
	}

	@Override
	public IntegerStruct logOrC2(final IntegerStruct integer) {
		if (integer instanceof FixnumStructImpl) {
			return IntegerStruct.toLispInteger(value | ~((FixnumStructImpl) integer).value);
		} else if (integer instanceof LongnumStructImpl) {
			return IntegerStruct.toLispInteger(value | ~((LongnumStructImpl) integer).value);
		} else {
			final BigInteger bigInteger1 = BigInteger.valueOf(value);
			final BigInteger bigInteger2 = ((BignumStructImpl) integer).value;
			return IntegerStruct.toLispInteger(bigInteger1.or(bigInteger2.not()));
		}
	}

	@Override
	public IntegerStruct logXor(final IntegerStruct integer) {
		if (integer instanceof FixnumStructImpl) {
			return IntegerStruct.toLispInteger(value ^ ((FixnumStructImpl) integer).value);
		} else if (integer instanceof LongnumStructImpl) {
			return IntegerStruct.toLispInteger(value ^ ((LongnumStructImpl) integer).value);
		} else {
			final BigInteger bigInteger1 = BigInteger.valueOf(value);
			final BigInteger bigInteger2 = ((BignumStructImpl) integer).value;
			return IntegerStruct.toLispInteger(bigInteger1.xor(bigInteger2));
		}
	}

	@Override
	public boolean logBitP(final IntegerStruct index) {
		final BigInteger bigInteger = BigInteger.valueOf(value);
		final int indexInt = index.toJavaInt();
		return bigInteger.testBit(indexInt);
	}

	@Override
	public IntegerStruct logCount() {
		final BigInteger bigInteger = BigInteger.valueOf(value);
		final int bitCount = bigInteger.bitCount();
		return IntegerStruct.toLispInteger(bitCount);
	}

	@Override
	public boolean logTest(final IntegerStruct integer) {
		final BigInteger bigInteger1 = BigInteger.valueOf(value);
		final BigInteger bigInteger2 = integer.toJavaBigInteger();
		final BigInteger and = bigInteger1.and(bigInteger2);
		return and.signum() != 0;
	}

	@Override
	public IntegerStruct integerLength() {
		final BigInteger bigInteger = BigInteger.valueOf(value);
		final int bitLength = bigInteger.bitLength();
		return IntegerStruct.toLispInteger(bitLength);
	}

	@Override
	public boolean evenp() {
		return (value & 0x01) == 0;
	}

	@Override
	public boolean oddp() {
		return (value & 0x01) != 0;
	}

	@SuppressWarnings("NumericCastThatLosesPrecision")
	@Override
	public NumberStruct isqrt() {
		if (value < 0) {
			final Apint ap = new Apint(value);
			final Apcomplex sqrt = ApcomplexMath.sqrt(ap);
			return ApfloatUtils.toNumberStruct(sqrt);
		}

		final long isqrt = LongMath.sqrt(value, RoundingMode.FLOOR);
		return IntegerStruct.toLispInteger(isqrt);
	}

	@Override
	public int toJavaInt() {
		return Long.valueOf(value).intValue();
	}

	@Override
	public Integer toJavaInteger() {
		return Long.valueOf(value).intValue();
	}

	@Override
	public long toJavaPLong() {
		return value;
	}

	@Override
	public Long toJavaLong() {
		return value;
	}

	@Override
	public BigInteger toJavaBigInteger() {
		return BigInteger.valueOf(value);
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
		if (real instanceof FixnumStructImpl) {
			return value < ((FixnumStructImpl) real).value;
		} else if (real instanceof LongnumStructImpl) {
			return value < ((LongnumStructImpl) real).value;
		} else if (real instanceof BignumStructImpl) {
			final BigInteger bigInteger1 = BigInteger.valueOf(value);
			final BigInteger bigInteger2 = ((BignumStructImpl) real).value;
			return bigInteger1.compareTo(bigInteger2) < 0;
		} else if (real instanceof RatioStructImpl) {
			final BigInteger bigInteger = BigInteger.valueOf(value);
			final BigFraction bigFraction1 = new BigFraction(bigInteger);
			final BigFraction bigFraction2 = ((RatioStructImpl) real).value;
			return bigFraction1.compareTo(bigFraction2) < 0;
		}
		return isGreaterThan(real.rational());
	}

	@Override
	public boolean isGreaterThan(final RealStruct real) {
		if (real instanceof FixnumStructImpl) {
			return value > ((FixnumStructImpl) real).value;
		} else if (real instanceof LongnumStructImpl) {
			return value > ((LongnumStructImpl) real).value;
		} else if (real instanceof BignumStructImpl) {
			final BigInteger bigInteger1 = BigInteger.valueOf(value);
			final BigInteger bigInteger2 = ((BignumStructImpl) real).value;
			return bigInteger1.compareTo(bigInteger2) > 0;
		} else if (real instanceof RatioStructImpl) {
			final BigInteger bigInteger = BigInteger.valueOf(value);
			final BigFraction bigFraction1 = new BigFraction(bigInteger);
			final BigFraction bigFraction2 = ((RatioStructImpl) real).value;
			return bigFraction1.compareTo(bigFraction2) > 0;
		}
		return isGreaterThan(real.rational());
	}

	@Override
	public boolean isLessThanOrEqualTo(final RealStruct real) {
		if (real instanceof FixnumStructImpl) {
			return value <= ((FixnumStructImpl) real).value;
		} else if (real instanceof LongnumStructImpl) {
			return value <= ((LongnumStructImpl) real).value;
		} else if (real instanceof BignumStructImpl) {
			final BigInteger bigInteger1 = BigInteger.valueOf(value);
			final BigInteger bigInteger2 = ((BignumStructImpl) real).value;
			return bigInteger1.compareTo(bigInteger2) <= 0;
		} else if (real instanceof RatioStructImpl) {
			final BigInteger bigInteger = BigInteger.valueOf(value);
			final BigFraction bigFraction1 = new BigFraction(bigInteger);
			final BigFraction bigFraction2 = ((RatioStructImpl) real).value;
			return bigFraction1.compareTo(bigFraction2) <= 0;
		}
		return isGreaterThan(real.rational());
	}

	@Override
	public boolean isGreaterThanOrEqualTo(final RealStruct real) {
		if (real instanceof FixnumStructImpl) {
			return value >= ((FixnumStructImpl) real).value;
		} else if (real instanceof LongnumStructImpl) {
			return value >= ((LongnumStructImpl) real).value;
		} else if (real instanceof BignumStructImpl) {
			final BigInteger bigInteger1 = BigInteger.valueOf(value);
			final BigInteger bigInteger2 = ((BignumStructImpl) real).value;
			return bigInteger1.compareTo(bigInteger2) >= 0;
		} else if (real instanceof RatioStructImpl) {
			final BigInteger bigInteger = BigInteger.valueOf(value);
			final BigFraction bigFraction1 = new BigFraction(bigInteger);
			final BigFraction bigFraction2 = ((RatioStructImpl) real).value;
			return bigFraction1.compareTo(bigFraction2) >= 0;
		}
		return isGreaterThan(real.rational());
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
	public FloatStruct floatingPoint() {
		return new SingleFloatStructImpl(value);
	}

	@Override
	public FloatStruct floatingPoint(final FloatStruct prototype) {
		if (prototype instanceof SingleFloatStruct) {
			return new SingleFloatStructImpl(value);
		} else {
			return new DoubleFloatStructImpl(value);
		}
	}

	@Override
	public RealStruct mod(final RealStruct divisor) {
		// TODO
		if (divisor instanceof FixnumStructImpl) {
			final long mod = value % ((FixnumStructImpl) divisor).value;
			return IntegerStruct.toLispInteger(mod);
		} else if (divisor instanceof LongnumStructImpl) {
			final long mod = value % ((LongnumStructImpl) divisor).value;
			return IntegerStruct.toLispInteger(mod);
		}
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
			final int divisorValue = ((FixnumStructImpl) divisor).value;
			final long quotient = value / divisorValue;
			final long remainder = value % divisorValue;

			return new QuotientRemainder(
					IntegerStruct.toLispInteger(quotient),
					IntegerStruct.toLispInteger(remainder)
			);
		} else if (divisor instanceof LongnumStructImpl) {
			final long divisorValue = ((LongnumStructImpl) divisor).value;
			final long quotient = value / divisorValue;
			final long remainder = value % divisorValue;

			return new QuotientRemainder(
					IntegerStruct.toLispInteger(quotient),
					IntegerStruct.toLispInteger(remainder)
			);
		} else if (divisor instanceof BignumStructImpl) {
			final BigInteger val = BigInteger.valueOf(value);
			final BigInteger divisorValue = ((BignumStructImpl) divisor).value;
			final BigInteger[] results = val.divideAndRemainder(divisorValue);
			final BigInteger quotient = results[0];
			final BigInteger remainder = results[1];

			return new QuotientRemainder(
					IntegerStruct.toLispInteger(quotient),
					IntegerStruct.toLispInteger(remainder)
			);
		} else if (divisor instanceof RatioStructImpl) {
			final BigInteger val = BigInteger.valueOf(value);
			final BigFraction divisorValue = ((RatioStructImpl) divisor).value;
			final BigInteger numerator = divisorValue.getNumerator();
			final BigInteger denominator = divisorValue.getDenominator();

			final BigInteger quotient = val.multiply(denominator).divide(numerator);

			final BigFraction valBF = new BigFraction(value);
			final BigFraction quotientBF = new BigFraction(quotient);
			final BigFraction remainder = valBF.subtract(quotientBF.multiply(divisorValue));

			return new QuotientRemainder(
					IntegerStruct.toLispInteger(quotient),
					RationalStruct.toLispRational(remainder)
			);
		} else if (divisor instanceof SingleFloatStructImpl) {
			final float divisorValue = ((SingleFloatStructImpl) divisor).value;
			final double divide = value / divisorValue;

			final long quotient = operation.apply(divide);
			final double remainder = value - (quotient * divisorValue);

			return new QuotientRemainder(
					quotientCreator.apply(quotient),
					new SingleFloatStructImpl(remainder)
			);
		} else if (divisor instanceof DoubleFloatStructImpl) {
			final double divisorValue = ((DoubleFloatStructImpl) divisor).value;
			final double divide = value / divisorValue;

			final long quotient = operation.apply(divide);
			final double remainder = value - (quotient * divisorValue);

			return new QuotientRemainder(
					quotientCreator.apply(quotient),
					new DoubleFloatStructImpl(remainder)
			);
		} else {
			final double divisorValue = divisor.ap().doubleValue();
			final double divide = value / divisorValue;

			final long quotient = operation.apply(divide);
			final double remainder = value - (quotient * divisorValue);

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
		return new QuotientRemainder(new SingleFloatStructImpl(value), ZERO);
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
		return new QuotientRemainder(new SingleFloatStructImpl(value), ZERO);
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
		return calculateQuotientRemainder(divisor, val -> DoubleMath.roundToLong(value, RoundingMode.DOWN),
		                                  IntegerStruct::toLispInteger);
	}

	@Override
	public QuotientRemainder ftruncate() {
		return new QuotientRemainder(new SingleFloatStructImpl(value), ZERO);
	}

	@Override
	public QuotientRemainder ftruncate(final RealStruct divisor) {
		return calculateQuotientRemainder(divisor, val -> DoubleMath.roundToLong(value, RoundingMode.DOWN),
		                                  toLispFloat(divisor));
	}

	@Override
	public QuotientRemainder round() {
		return new QuotientRemainder(this, ZERO);
	}

	@Override
	public QuotientRemainder round(final RealStruct divisor) {
		return calculateQuotientRemainder(divisor, val -> DoubleMath.roundToLong(value, RoundingMode.HALF_EVEN),
		                                  IntegerStruct::toLispInteger);
	}

	@Override
	public QuotientRemainder fround() {
		return new QuotientRemainder(new SingleFloatStructImpl(value), ZERO);
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
			return new SingleFloatStructImpl(atan2);
		} else if (real instanceof LongnumStructImpl) {
			final double atan2 = StrictMath.atan2(value, ((LongnumStructImpl) real).value);
			return new SingleFloatStructImpl(atan2);
		} else if (real instanceof SingleFloatStructImpl) {
			final double atan2 = StrictMath.atan2(value, ((SingleFloatStructImpl) real).value);
			return new SingleFloatStructImpl(atan2);
		} else if (real instanceof DoubleFloatStructImpl) {
			final double atan2 = StrictMath.atan2(value, ((DoubleFloatStructImpl) real).value);
			return new SingleFloatStructImpl(atan2);
		}
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
		return new LongnumStructImpl(Math.abs(value));
	}

	@Override
	public boolean zerop() {
		return value == 0;
	}

	@Override
	public NumberStruct add(final NumberStruct number) {
		if (number instanceof FixnumStructImpl) {
			return addExact(value, ((FixnumStructImpl) number).value);
		} else if (number instanceof LongnumStructImpl) {
			return addExact(value, ((LongnumStructImpl) number).value);
		} else if (number instanceof BignumStructImpl) {
			final BigInteger add = BigInteger.valueOf(value).add(((BignumStructImpl) number).value);
			return new BignumStructImpl(add);
		} else if (number instanceof RatioStructImpl) {
			final BigFraction add = ((RatioStructImpl) number).value.add(value);
			return new RatioStructImpl(add);
		} else if (number instanceof SingleFloatStructImpl) {
			final float add = value + ((SingleFloatStructImpl) number).value;
			return new SingleFloatStructImpl(add);
		} else if (number instanceof DoubleFloatStructImpl) {
			final double add = value + ((DoubleFloatStructImpl) number).value;
			return new DoubleFloatStructImpl(add);
		}
		final Apint ap = new Apint(value);
		final Apcomplex numberAp = number.ap();
		final Apcomplex add = ap.add(numberAp);
		return ApfloatUtils.toNumberStruct(add);
	}

	@Override
	public NumberStruct subtract(final NumberStruct number) {
		if (number instanceof FixnumStructImpl) {
			return subtractExact(value, ((FixnumStructImpl) number).value);
		} else if (number instanceof LongnumStructImpl) {
			return subtractExact(value, ((LongnumStructImpl) number).value);
		} else if (number instanceof BignumStructImpl) {
			final BigInteger subtract = BigInteger.valueOf(value).subtract(((BignumStructImpl) number).value);
			return new BignumStructImpl(subtract);
		} else if (number instanceof RatioStructImpl) {
			final BigFraction subtract = new BigFraction(value).subtract(((RatioStructImpl) number).value);
			return new RatioStructImpl(subtract);
		} else if (number instanceof SingleFloatStructImpl) {
			final float subtract = value - ((SingleFloatStructImpl) number).value;
			return new SingleFloatStructImpl(subtract);
		} else if (number instanceof DoubleFloatStructImpl) {
			final double subtract = value - ((DoubleFloatStructImpl) number).value;
			return new DoubleFloatStructImpl(subtract);
		}
		final Apint ap = new Apint(value);
		final Apcomplex numberAp = number.ap();
		final Apcomplex subtract = ap.subtract(numberAp);
		return ApfloatUtils.toNumberStruct(subtract);
	}

	@Override
	public NumberStruct multiply(final NumberStruct number) {
		if (number instanceof FixnumStructImpl) {
			return multiplyExact(value, ((FixnumStructImpl) number).value);
		} else if (number instanceof LongnumStructImpl) {
			return multiplyExact(value, ((LongnumStructImpl) number).value);
		} else if (number instanceof BignumStructImpl) {
			final BigInteger multiply = BigInteger.valueOf(value).multiply(((BignumStructImpl) number).value);
			return new BignumStructImpl(multiply);
		} else if (number instanceof RatioStructImpl) {
			final BigFraction multiply = ((RatioStructImpl) number).value.multiply(value);
			return RationalStruct.toLispRational(multiply);
		} else if (number instanceof SingleFloatStructImpl) {
			final float multiply = value * ((SingleFloatStructImpl) number).value;
			return new SingleFloatStructImpl(multiply);
		} else if (number instanceof DoubleFloatStructImpl) {
			final double multiply = value * ((DoubleFloatStructImpl) number).value;
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
			final float divide = value / ((SingleFloatStructImpl) number).value;
			return new SingleFloatStructImpl(divide);
		} else if (number instanceof DoubleFloatStructImpl) {
			final double divide = value / ((DoubleFloatStructImpl) number).value;
			return new DoubleFloatStructImpl(divide);
		}
		final Apint ap = new Apint(value);
		final Apcomplex numberAp = number.ap();
		final Apcomplex divide = ap.divide(numberAp);
		return ApfloatUtils.toNumberStruct(divide);
	}

	@Override
	public boolean isEqualTo(final NumberStruct number) {
		if (number instanceof FixnumStructImpl) {
			return value == ((FixnumStructImpl) number).value;
		} else if (number instanceof LongnumStructImpl) {
			return value == ((LongnumStructImpl) number).value;
		} else if (number instanceof BignumStructImpl) {
			final BigInteger bigInteger1 = BigInteger.valueOf(value);
			final BigInteger bigInteger2 = ((BignumStructImpl) number).value;
			return bigInteger1.compareTo(bigInteger2) == 0;
		}
		return number.isEqualTo(this);
	}

	@Override
	public boolean isNotEqualTo(final NumberStruct number) {
		if (number instanceof FixnumStructImpl) {
			return value != ((FixnumStructImpl) number).value;
		} else if (number instanceof LongnumStructImpl) {
			return value != ((LongnumStructImpl) number).value;
		} else if (number instanceof BignumStructImpl) {
			final BigInteger bigInteger1 = BigInteger.valueOf(value);
			final BigInteger bigInteger2 = ((BignumStructImpl) number).value;
			return bigInteger1.compareTo(bigInteger2) != 0;
		}
		return number.isNotEqualTo(this);
	}

	@Override
	public IntegerStruct signum() {
		if (value == 0) {
			return this;
		}
		if (value > 0) {
			return ONE;
		}
		return MINUS_ONE;
	}

	@Override
	public IntegerStruct negation() {
		return new LongnumStructImpl(-value);
	}

	@Override
	public RationalStruct reciprocal() {
		if (value == 1) {
			return this;
		}
		return new RatioStructImpl(ONE, this);
	}

	@Override
	public RealStruct exp() {
		final double exp = StrictMath.exp(value);
		return new SingleFloatStructImpl(exp);
	}

	@Override
	public NumberStruct expt(final NumberStruct power) {
		if (power instanceof IntegerStruct) {
			final BigInteger bigInteger1 = BigInteger.valueOf(value);
			final BigInteger bigInteger2 = ((IntegerStruct) power).toJavaBigInteger();
			final BigInteger pow = ArithmeticUtils.pow(bigInteger1, bigInteger2);
			return IntegerStruct.toLispInteger(pow);
		}
		final Apint ap = new Apint(value);
		final Apcomplex powerAp = power.ap();
		final Apcomplex pow = ApcomplexMath.pow(ap, powerAp);
		return ApfloatUtils.toNumberStruct(pow);
	}

	@Override
	public RealStruct log() {
		final double log = StrictMath.log(value);
		return new SingleFloatStructImpl(log);
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
		if (value < 0) {
			final Apint ap = new Apint(value);
			final Apcomplex sqrt = ApcomplexMath.sqrt(ap);
			return ApfloatUtils.toNumberStruct(sqrt);
		}

		final double sqrt = StrictMath.sqrt(value);
		return new SingleFloatStructImpl(sqrt);
	}

	@Override
	public RealStruct sin() {
		final double sin = StrictMath.sin(value);
		return new SingleFloatStructImpl(sin);
	}

	@Override
	public RealStruct cos() {
		final double cos = StrictMath.cos(value);
		return new SingleFloatStructImpl(cos);
	}

	@Override
	public RealStruct tan() {
		final double tan = StrictMath.tan(value);
		return new SingleFloatStructImpl(tan);
	}

	@Override
	public RealStruct asin() {
		final double asin = StrictMath.asin(value);
		return new SingleFloatStructImpl(asin);
	}

	@Override
	public RealStruct acos() {
		final double acos = StrictMath.acos(value);
		return new SingleFloatStructImpl(acos);
	}

	@Override
	public RealStruct atan() {
		final double atan = StrictMath.atan(value);
		return new SingleFloatStructImpl(atan);
	}

	@Override
	public RealStruct sinh() {
		final double sinh = StrictMath.sinh(value);
		return new SingleFloatStructImpl(sinh);
	}

	@Override
	public RealStruct cosh() {
		final double cosh = StrictMath.cosh(value);
		return new SingleFloatStructImpl(cosh);
	}

	@Override
	public RealStruct tanh() {
		final double tanh = StrictMath.tanh(value);
		return new SingleFloatStructImpl(tanh);
	}

	@Override
	public RealStruct asinh() {
		final double asinh = FastMath.asinh(value);
		return new SingleFloatStructImpl(asinh);
	}

	@Override
	public RealStruct acosh() {
		final double acosh = FastMath.acosh(value);
		return new SingleFloatStructImpl(acosh);
	}

	@Override
	public RealStruct atanh() {
		final double atanh = FastMath.atanh(value);
		return new SingleFloatStructImpl(atanh);
	}
}
