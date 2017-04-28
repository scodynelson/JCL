package jcl.lang.internal;

import java.math.BigDecimal;
import java.math.BigInteger;

import jcl.lang.BignumStruct;
import jcl.lang.DoubleFloatStruct;
import jcl.lang.FloatStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.NumberStruct;
import jcl.lang.RationalStruct;
import jcl.lang.RealStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.internal.number.FloatStructImpl;
import jcl.lang.internal.number.RatioStructImpl;
import jcl.lang.number.QuotientRemainder;
import jcl.type.BignumType;
import lombok.EqualsAndHashCode;
import org.apache.commons.math3.fraction.BigFraction;
import org.apfloat.Apcomplex;
import org.apfloat.ApcomplexMath;
import org.apfloat.Apfloat;
import org.apfloat.ApfloatMath;
import org.apfloat.Apint;
import org.apfloat.ApintMath;

@EqualsAndHashCode(callSuper = true)
public final class BignumStructImpl extends BuiltInClassStruct implements BignumStruct {

	private final BigInteger value;

	public BignumStructImpl(final BigInteger value) {
		super(BignumType.INSTANCE, null, null);
		this.value = value;
	}

	@Override
	public int intValue() {
		return value.intValue();
	}

	@Override
	public long longValue() {
		return value.longValue();
	}

	@Override
	public BigInteger bigIntegerValue() {
		return value;
	}

	@Override
	public IntegerStruct gcd(final IntegerStruct integer) {
		final BigInteger gcd = value.gcd(integer.bigIntegerValue());
		return IntegerStruct.toLispInteger(gcd);
	}

	@Override
	public IntegerStruct lcm(final IntegerStruct integer) {
		final Apint lcm = ApintMath.lcm(ap(), integer.ap());
		return IntegerStruct.toLispInteger(lcm.toBigInteger());
	}

	@Override
	public IntegerStruct ash(final IntegerStruct count) {
		final Apint countAp = count.ap();
		if (countAp.signum() == 0) {
			return this;
		}
		final int countI = countAp.intValue();

		// NOTE: shiftLeft will automatically take care of shiftRight based on the sign of countInt
		final BigInteger shiftedBigInteger = value.shiftLeft(countI);
		return IntegerStruct.toLispInteger(shiftedBigInteger);
	}

	@Override
	public IntegerStruct logAnd(final IntegerStruct integer) {
		final BigInteger bigInteger = integer.bigIntegerValue();
		return IntegerStruct.toLispInteger(value.and(bigInteger));
	}

	@Override
	public IntegerStruct logAndC1(final IntegerStruct integer) {
		final BigInteger bigInteger = integer.bigIntegerValue();
		return IntegerStruct.toLispInteger(value.not().and(bigInteger));
	}

	@Override
	public IntegerStruct logAndC2(final IntegerStruct integer) {
		final BigInteger bigInteger = integer.bigIntegerValue();
		return IntegerStruct.toLispInteger(value.and(bigInteger.not()));
	}

	@Override
	public IntegerStruct logEqv(final IntegerStruct integer) {
		final BigInteger bigInteger = integer.bigIntegerValue();
		final BigInteger xor = value.xor(bigInteger);
		return IntegerStruct.toLispInteger(xor.not());
	}

	@Override
	public IntegerStruct logIor(final IntegerStruct integer) {
		final BigInteger bigInteger = integer.bigIntegerValue();
		return IntegerStruct.toLispInteger(value.or(bigInteger));
	}

	@Override
	public IntegerStruct logNand(final IntegerStruct integer) {
		final BigInteger bigInteger = integer.bigIntegerValue();
		final BigInteger and = value.and(bigInteger);
		return IntegerStruct.toLispInteger(and.not());
	}

	@Override
	public IntegerStruct logNor(final IntegerStruct integer) {
		final BigInteger bigInteger = integer.bigIntegerValue();
		final BigInteger or = value.or(bigInteger);
		return IntegerStruct.toLispInteger(or.not());
	}

	@Override
	public IntegerStruct logNot() {
		return IntegerStruct.toLispInteger(value.not());
	}

	@Override
	public IntegerStruct logOrC1(final IntegerStruct integer) {
		final BigInteger bigInteger = integer.bigIntegerValue();
		return IntegerStruct.toLispInteger(value.not().or(bigInteger));
	}

	@Override
	public IntegerStruct logOrC2(final IntegerStruct integer) {
		final BigInteger bigInteger = integer.bigIntegerValue();
		return IntegerStruct.toLispInteger(value.or(bigInteger.not()));
	}

	@Override
	public IntegerStruct logXor(final IntegerStruct integer) {
		final BigInteger bigInteger = integer.bigIntegerValue();
		return IntegerStruct.toLispInteger(value.xor(bigInteger));
	}

	@Override
	public boolean logBitP(final IntegerStruct index) {
		final int indexInt = index.intValue();
		return value.testBit(indexInt);
	}

	@Override
	public IntegerStruct logCount() {
		final int bitCount = value.bitCount();
		return IntegerStruct.toLispInteger(bitCount);
	}

	@Override
	public boolean logTest(final IntegerStruct integer) {
		final BigInteger bigInteger = integer.bigIntegerValue();
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
	public IntegerStruct isqrt() {
		final Apint ap = new Apint(value);
		final Apint[] sqrt = ApintMath.sqrt(ap);
		final BigInteger isqrt = sqrt[0].toBigInteger();
		return IntegerStruct.toLispInteger(isqrt);
	}

	/*
	REAL-STRUCT
	 */

	@Override
	public boolean isLessThan(final RealStruct real) {
		if (real instanceof IntegerStruct) {
			final BigInteger bigInteger = ((IntegerStruct) real).bigIntegerValue();
			return value.compareTo(bigInteger) < 0;
		} else if (real instanceof RatioStructImpl) {
			final BigFraction bigFraction1 = new BigFraction(value);
			final BigFraction bigFraction2 = ((RatioStructImpl) real).toBigFraction();
			return bigFraction1.compareTo(bigFraction2) < 0;
		}
		return isLessThan(real.rational());
	}

	@Override
	public boolean isGreaterThan(final RealStruct real) {
		if (real instanceof IntegerStruct) {
			final BigInteger bigInteger = ((IntegerStruct) real).bigIntegerValue();
			return value.compareTo(bigInteger) > 0;
		} else if (real instanceof RatioStructImpl) {
			final BigFraction bigFraction1 = new BigFraction(value);
			final BigFraction bigFraction2 = ((RatioStructImpl) real).toBigFraction();
			return bigFraction1.compareTo(bigFraction2) > 0;
		}
		return isGreaterThan(real.rational());
	}

	@Override
	public boolean isLessThanOrEqualTo(final RealStruct real) {
		if (real instanceof IntegerStruct) {
			final BigInteger bigInteger = ((IntegerStruct) real).bigIntegerValue();
			return value.compareTo(bigInteger) <= 0;
		} else if (real instanceof RatioStructImpl) {
			final BigFraction bigFraction1 = new BigFraction(value);
			final BigFraction bigFraction2 = ((RatioStructImpl) real).toBigFraction();
			return bigFraction1.compareTo(bigFraction2) <= 0;
		}
		return isLessThanOrEqualTo(real.rational());
	}

	@Override
	public boolean isGreaterThanOrEqualTo(final RealStruct real) {
		if (real instanceof IntegerStruct) {
			final BigInteger bigInteger = ((IntegerStruct) real).bigIntegerValue();
			return value.compareTo(bigInteger) >= 0;
		} else if (real instanceof RatioStructImpl) {
			final BigFraction bigFraction1 = new BigFraction(value);
			final BigFraction bigFraction2 = ((RatioStructImpl) real).toBigFraction();
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
		// TODO
		final BigDecimal bigDecimal = new BigDecimal(value);
		return FloatStructImpl.valueOf(bigDecimal);
	}

	@Override
	public FloatStruct floatingPoint(final FloatStruct prototype) {
		// TODO
		final BigDecimal bigDecimal = new BigDecimal(value);
		return FloatStructImpl.valueOf(new Apfloat(bigDecimal), prototype);
	}

	@Override
	public RealStruct mod(final RealStruct divisor) {
		final QuotientRemainder floor = floor(divisor);
		return floor.getRemainder();
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
		final Apint ap = new Apint(value);
		final Apfloat realAp = real.ap();
		final Apfloat atan2 = ApfloatMath.atan2(ap, realAp);
		return DoubleFloatStruct.toLispFloat(atan2.doubleValue());
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
		return IntegerStruct.toLispInteger(value.abs());
	}

	@Override
	public boolean zerop() {
		return value.signum() == 0;
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
		if (number instanceof IntegerStruct) {
			final BigInteger bigInteger = ((IntegerStruct) number).bigIntegerValue();
			return value.compareTo(bigInteger) == 0;
		}
		return number.isEqualTo(this);
	}

	@Override
	public boolean isNotEqualTo(final NumberStruct number) {
		if (number instanceof IntegerStruct) {
			final BigInteger bigInteger = ((IntegerStruct) number).bigIntegerValue();
			return value.compareTo(bigInteger) != 0;
		}
		return number.isNotEqualTo(this);
	}

	@Override
	public IntegerStruct signum() {
		return IntegerStruct.toLispInteger(value.signum());
	}

	@Override
	public IntegerStruct negation() {
		return IntegerStruct.toLispInteger(value.negate());
	}

	@Override
	public RationalStruct reciprocal() {
		// TODO
		return null;
	}

	@Override
	public RealStruct exp() {
		// TODO
		final Apint ap = new Apint(value);
		final Apfloat exp = ApfloatMath.exp(ap);
		return RealStruct.valueOf(exp);
	}

	@Override
	public NumberStruct expt(final NumberStruct power) {
		// TODO
		final Apint ap = new Apint(value);
		final Apcomplex powerAp = power.ap();
		final Apcomplex pow = ApcomplexMath.pow(ap, powerAp);
		return NumberStruct.valueOf(pow);
	}

	@Override
	public RealStruct log() {
		// TODO
		final Apint ap = new Apint(value);
		final Apfloat log = ApfloatMath.log(ap);
		return RealStruct.valueOf(log);
	}

	@Override
	public NumberStruct log(final NumberStruct base) {
		// TODO
		final Apint ap = new Apint(value);
		final Apcomplex baseAp = base.ap();
		if (baseAp instanceof Apfloat) {
			final Apfloat log = ApfloatMath.log(ap, (Apfloat) baseAp);
			return RealStruct.valueOf(log);
		}
		final Apcomplex log = ApcomplexMath.log(ap, baseAp);
		return NumberStruct.valueOf(log);
	}

	@Override
	public RealStruct sqrt() {
		// TODO
		final Apint ap = new Apint(value);
		final Apfloat sqrt = ApfloatMath.sqrt(ap);
		return RealStruct.valueOf(sqrt);
	}

	@Override
	public RealStruct sin() {
		// TODO
		final Apint ap = new Apint(value);
		final Apfloat sin = ApfloatMath.sin(ap);
		return RealStruct.valueOf(sin);
	}

	@Override
	public RealStruct cos() {
		// TODO
		final Apint ap = new Apint(value);
		final Apfloat cos = ApfloatMath.cos(ap);
		return RealStruct.valueOf(cos);
	}

	@Override
	public RealStruct tan() {
		// TODO
		final Apint ap = new Apint(value);
		final Apfloat tan = ApfloatMath.tan(ap);
		return RealStruct.valueOf(tan);
	}

	@Override
	public RealStruct asin() {
		// TODO
		final Apint ap = new Apint(value);
		final Apfloat asin = ApfloatMath.asin(ap);
		return RealStruct.valueOf(asin);
	}

	@Override
	public RealStruct acos() {
		// TODO
		final Apint ap = new Apint(value);
		final Apfloat acos = ApfloatMath.acos(ap);
		return RealStruct.valueOf(acos);
	}

	@Override
	public RealStruct atan() {
		// TODO
		final Apint ap = new Apint(value);
		final Apfloat atan = ApfloatMath.atan(ap);
		return RealStruct.valueOf(atan);
	}

	@Override
	public RealStruct sinh() {
		// TODO
		final Apint ap = new Apint(value);
		final Apfloat sinh = ApfloatMath.sinh(ap);
		return RealStruct.valueOf(sinh);
	}

	@Override
	public RealStruct cosh() {
		// TODO
		final Apint ap = new Apint(value);
		final Apfloat cosh = ApfloatMath.cosh(ap);
		return RealStruct.valueOf(cosh);
	}

	@Override
	public RealStruct tanh() {
		// TODO
		final Apint ap = new Apint(value);
		final Apfloat tanh = ApfloatMath.tanh(ap);
		return RealStruct.valueOf(tanh);
	}

	@Override
	public RealStruct asinh() {
		// TODO
		final Apint ap = new Apint(value);
		final Apfloat asinh = ApfloatMath.asinh(ap);
		return RealStruct.valueOf(asinh);
	}

	@Override
	public RealStruct acosh() {
		// TODO
		final Apint ap = new Apint(value);
		final Apfloat acosh = ApfloatMath.acosh(ap);
		return RealStruct.valueOf(acosh);
	}

	@Override
	public RealStruct atanh() {
		// TODO
		final Apint ap = new Apint(value);
		final Apfloat atanh = ApfloatMath.atanh(ap);
		return RealStruct.valueOf(atanh);
	}
}
