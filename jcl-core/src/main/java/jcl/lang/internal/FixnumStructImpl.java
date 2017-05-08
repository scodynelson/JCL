package jcl.lang.internal;

import java.math.BigInteger;

import jcl.lang.DoubleFloatStruct;
import jcl.lang.FixnumStruct;
import jcl.lang.FloatStruct;
import jcl.lang.IntegerStruct;
import jcl.lang.NumberStruct;
import jcl.lang.RationalStruct;
import jcl.lang.RealStruct;
import jcl.lang.SingleFloatStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.lang.internal.number.FloatStructImpl;
import jcl.lang.internal.number.RatioStructImpl;
import jcl.lang.number.QuotientRemainder;
import jcl.type.FixnumType;
import lombok.EqualsAndHashCode;
import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.util.ArithmeticUtils;
import org.apache.commons.math3.util.FastMath;
import org.apfloat.Apcomplex;
import org.apfloat.ApcomplexMath;
import org.apfloat.Apfloat;
import org.apfloat.ApfloatMath;
import org.apfloat.Apint;
import org.apfloat.ApintMath;

@EqualsAndHashCode(callSuper = true)
public final class FixnumStructImpl extends BuiltInClassStruct implements FixnumStruct {

	private final int value;

	public FixnumStructImpl(final int value) {
		super(FixnumType.INSTANCE, null, null);
		this.value = value;
	}

	@Override
	public int intValue() {
		return value;
	}

	@Override
	public long longValue() {
		return value;
	}

	@Override
	public BigInteger bigIntegerValue() {
		return BigInteger.valueOf(value);
	}

	@Override
	public IntegerStruct gcd(final IntegerStruct integer) {
		if (integer instanceof FixnumStructImpl) {
			final int gcd = ArithmeticUtils.gcd(value, integer.intValue());
			return IntegerStruct.toLispInteger(gcd);
		} else if (integer instanceof LongnumStructImpl) {
			final long gcd = ArithmeticUtils.gcd(value, integer.longValue());
			return IntegerStruct.toLispInteger(gcd);
		} else {
			final BigInteger gcd = BigInteger.valueOf(value).gcd(integer.bigIntegerValue());
			return IntegerStruct.toLispInteger(gcd);
		}
	}

	@Override
	public IntegerStruct lcm(final IntegerStruct integer) {
		if (integer instanceof FixnumStructImpl) {
			final int lcm = ArithmeticUtils.lcm(value, integer.intValue());
			return IntegerStruct.toLispInteger(lcm);
		} else if (integer instanceof LongnumStructImpl) {
			final long lcm = ArithmeticUtils.lcm(value, integer.longValue());
			return IntegerStruct.toLispInteger(lcm);
		} else {
			final Apint lcm = ApintMath.lcm(ap(), integer.ap());
			return IntegerStruct.toLispInteger(lcm.toBigInteger());
		}
	}

	@Override
	public IntegerStruct ash(final IntegerStruct count) {
		final Apint countAp = count.ap();
		if (countAp.signum() == 0) {
			return this;
		}
		final int countI = countAp.intValue();

		// NOTE: shiftLeft will automatically take care of shiftRight based on the sign of countInt
		final BigInteger bigInteger = BigInteger.valueOf(value);
		final BigInteger shiftedBigInteger = bigInteger.shiftLeft(countI);
		return IntegerStruct.toLispInteger(shiftedBigInteger);
	}

	@Override
	public IntegerStruct logAnd(final IntegerStruct integer) {
		if (integer instanceof FixnumStructImpl) {
			return IntegerStruct.toLispInteger(value & integer.intValue());
		} else if (integer instanceof LongnumStructImpl) {
			return IntegerStruct.toLispInteger(value & integer.longValue());
		} else {
			final BigInteger bigInteger1 = BigInteger.valueOf(value);
			final BigInteger bigInteger2 = integer.bigIntegerValue();
			return IntegerStruct.toLispInteger(bigInteger1.and(bigInteger2));
		}
	}

	@Override
	public IntegerStruct logAndC1(final IntegerStruct integer) {
		if (integer instanceof FixnumStructImpl) {
			return IntegerStruct.toLispInteger(~value & integer.intValue());
		} else if (integer instanceof LongnumStructImpl) {
			return IntegerStruct.toLispInteger(~value & integer.longValue());
		} else {
			final BigInteger bigInteger1 = BigInteger.valueOf(value);
			final BigInteger bigInteger2 = integer.bigIntegerValue();
			return IntegerStruct.toLispInteger(bigInteger1.not().and(bigInteger2));
		}
	}

	@Override
	public IntegerStruct logAndC2(final IntegerStruct integer) {
		if (integer instanceof FixnumStructImpl) {
			return IntegerStruct.toLispInteger(value & ~integer.intValue());
		} else if (integer instanceof LongnumStructImpl) {
			return IntegerStruct.toLispInteger(value & ~integer.longValue());
		} else {
			final BigInteger bigInteger1 = BigInteger.valueOf(value);
			final BigInteger bigInteger2 = integer.bigIntegerValue();
			return IntegerStruct.toLispInteger(bigInteger1.and(bigInteger2.not()));
		}
	}

	@Override
	public IntegerStruct logEqv(final IntegerStruct integer) {
		if (integer instanceof FixnumStructImpl) {
			return IntegerStruct.toLispInteger(~(value ^ integer.intValue()));
		} else if (integer instanceof LongnumStructImpl) {
			return IntegerStruct.toLispInteger(~(value ^ integer.longValue()));
		} else {
			final BigInteger bigInteger1 = BigInteger.valueOf(value);
			final BigInteger bigInteger2 = integer.bigIntegerValue();
			final BigInteger xor = bigInteger1.xor(bigInteger2);
			return IntegerStruct.toLispInteger(xor.not());
		}
	}

	@Override
	public IntegerStruct logIor(final IntegerStruct integer) {
		if (integer instanceof FixnumStructImpl) {
			return IntegerStruct.toLispInteger(value | integer.intValue());
		} else if (integer instanceof LongnumStructImpl) {
			return IntegerStruct.toLispInteger(value | integer.longValue());
		} else {
			final BigInteger bigInteger1 = BigInteger.valueOf(value);
			final BigInteger bigInteger2 = integer.bigIntegerValue();
			return IntegerStruct.toLispInteger(bigInteger1.or(bigInteger2));
		}
	}

	@Override
	public IntegerStruct logNand(final IntegerStruct integer) {
		if (integer instanceof FixnumStructImpl) {
			return IntegerStruct.toLispInteger(~(value & integer.intValue()));
		} else if (integer instanceof LongnumStructImpl) {
			return IntegerStruct.toLispInteger(~(value & integer.longValue()));
		} else {
			final BigInteger bigInteger1 = BigInteger.valueOf(value);
			final BigInteger bigInteger2 = integer.bigIntegerValue();
			final BigInteger and = bigInteger1.and(bigInteger2);
			return IntegerStruct.toLispInteger(and.not());
		}
	}

	@Override
	public IntegerStruct logNor(final IntegerStruct integer) {
		if (integer instanceof FixnumStructImpl) {
			return IntegerStruct.toLispInteger(~(value | integer.intValue()));
		} else if (integer instanceof LongnumStructImpl) {
			return IntegerStruct.toLispInteger(~(value | integer.longValue()));
		} else {
			final BigInteger bigInteger1 = BigInteger.valueOf(value);
			final BigInteger bigInteger2 = integer.bigIntegerValue();
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
			return IntegerStruct.toLispInteger(~value | integer.intValue());
		} else if (integer instanceof LongnumStructImpl) {
			return IntegerStruct.toLispInteger(~value | integer.longValue());
		} else {
			final BigInteger bigInteger1 = BigInteger.valueOf(value);
			final BigInteger bigInteger2 = integer.bigIntegerValue();
			return IntegerStruct.toLispInteger(bigInteger1.not().or(bigInteger2));
		}
	}

	@Override
	public IntegerStruct logOrC2(final IntegerStruct integer) {
		if (integer instanceof FixnumStructImpl) {
			return IntegerStruct.toLispInteger(value | ~integer.intValue());
		} else if (integer instanceof LongnumStructImpl) {
			return IntegerStruct.toLispInteger(value | ~integer.longValue());
		} else {
			final BigInteger bigInteger1 = BigInteger.valueOf(value);
			final BigInteger bigInteger2 = integer.bigIntegerValue();
			return IntegerStruct.toLispInteger(bigInteger1.or(bigInteger2.not()));
		}
	}

	@Override
	public IntegerStruct logXor(final IntegerStruct integer) {
		if (integer instanceof FixnumStructImpl) {
			return IntegerStruct.toLispInteger(value ^ integer.intValue());
		} else if (integer instanceof LongnumStructImpl) {
			return IntegerStruct.toLispInteger(value ^ integer.longValue());
		} else {
			final BigInteger bigInteger1 = BigInteger.valueOf(value);
			final BigInteger bigInteger2 = integer.bigIntegerValue();
			return IntegerStruct.toLispInteger(bigInteger1.xor(bigInteger2));
		}
	}

	@Override
	public boolean logBitP(final IntegerStruct index) {
		final BigInteger bigInteger = BigInteger.valueOf(value);
		final int indexInt = index.intValue();
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
		final BigInteger bigInteger2 = integer.bigIntegerValue();
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

	@Override
	public IntegerStruct isqrt() {
		final int isqrt = (int) Math.floor(Math.sqrt(value));
		return IntegerStruct.toLispInteger(isqrt);
	}

	/*
	REAL-STRUCT
	 */

	@Override
	public boolean isLessThan(final RealStruct real) {
		if (real instanceof FixnumStructImpl) {
			return value < ((FixnumStructImpl) real).intValue();
		} else if (real instanceof LongnumStructImpl) {
			return value < ((LongnumStructImpl) real).longValue();
		} else if (real instanceof BignumStructImpl) {
			final BigInteger bigInteger1 = BigInteger.valueOf(value);
			final BigInteger bigInteger2 = ((BignumStructImpl) real).bigIntegerValue();
			return bigInteger1.compareTo(bigInteger2) < 0;
		} else if (real instanceof RatioStructImpl) {
			final BigInteger bigInteger = BigInteger.valueOf(value);
			final BigFraction bigFraction1 = new BigFraction(bigInteger);
			final BigFraction bigFraction2 = ((RatioStructImpl) real).toBigFraction();
			return bigFraction1.compareTo(bigFraction2) < 0;
		}
		return isLessThan(real.rational());
	}

	@Override
	public boolean isGreaterThan(final RealStruct real) {
		if (real instanceof FixnumStructImpl) {
			return value > ((FixnumStructImpl) real).intValue();
		} else if (real instanceof LongnumStructImpl) {
			return value > ((LongnumStructImpl) real).longValue();
		} else if (real instanceof BignumStructImpl) {
			final BigInteger bigInteger1 = BigInteger.valueOf(value);
			final BigInteger bigInteger2 = ((BignumStructImpl) real).bigIntegerValue();
			return bigInteger1.compareTo(bigInteger2) > 0;
		} else if (real instanceof RatioStructImpl) {
			final BigInteger bigInteger = BigInteger.valueOf(value);
			final BigFraction bigFraction1 = new BigFraction(bigInteger);
			final BigFraction bigFraction2 = ((RatioStructImpl) real).toBigFraction();
			return bigFraction1.compareTo(bigFraction2) > 0;
		}
		return isGreaterThan(real.rational());
	}

	@Override
	public boolean isLessThanOrEqualTo(final RealStruct real) {
		if (real instanceof FixnumStructImpl) {
			return value <= ((FixnumStructImpl) real).intValue();
		} else if (real instanceof LongnumStructImpl) {
			return value <= ((LongnumStructImpl) real).longValue();
		} else if (real instanceof BignumStructImpl) {
			final BigInteger bigInteger1 = BigInteger.valueOf(value);
			final BigInteger bigInteger2 = ((BignumStructImpl) real).bigIntegerValue();
			return bigInteger1.compareTo(bigInteger2) <= 0;
		} else if (real instanceof RatioStructImpl) {
			final BigInteger bigInteger = BigInteger.valueOf(value);
			final BigFraction bigFraction1 = new BigFraction(bigInteger);
			final BigFraction bigFraction2 = ((RatioStructImpl) real).toBigFraction();
			return bigFraction1.compareTo(bigFraction2) <= 0;
		}
		return isLessThanOrEqualTo(real.rational());
	}

	@Override
	public boolean isGreaterThanOrEqualTo(final RealStruct real) {
		if (real instanceof FixnumStructImpl) {
			return value >= ((FixnumStructImpl) real).intValue();
		} else if (real instanceof LongnumStructImpl) {
			return value >= ((LongnumStructImpl) real).longValue();
		} else if (real instanceof BignumStructImpl) {
			final BigInteger bigInteger1 = BigInteger.valueOf(value);
			final BigInteger bigInteger2 = ((BignumStructImpl) real).bigIntegerValue();
			return bigInteger1.compareTo(bigInteger2) >= 0;
		} else if (real instanceof RatioStructImpl) {
			final BigInteger bigInteger = BigInteger.valueOf(value);
			final BigFraction bigFraction1 = new BigFraction(bigInteger);
			final BigFraction bigFraction2 = ((RatioStructImpl) real).toBigFraction();
			return bigFraction1.compareTo(bigFraction2) >= 0;
		}
		return isGreaterThanOrEqualTo(real.rational());
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
		// TODO
		return SingleFloatStruct.toLispFloat((float) value);
	}

	@Override
	public FloatStruct floatingPoint(final FloatStruct prototype) {
		// TODO
		return FloatStructImpl.valueOf(new Apfloat((float) value), prototype);
	}

	@Override
	public RealStruct mod(final RealStruct divisor) {
		if (divisor instanceof FixnumStructImpl) {
			final int mod = value % ((FixnumStructImpl) divisor).intValue();
			return IntegerStruct.toLispInteger(mod);
		} else if (divisor instanceof LongnumStructImpl) {
			final long mod = value % ((LongnumStructImpl) divisor).longValue();
			return IntegerStruct.toLispInteger(mod);
		}
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
		if (real instanceof FixnumStructImpl) {
			final double atan2 = StrictMath.atan2(value, ((FixnumStructImpl) real).intValue());
			return DoubleFloatStruct.toLispFloat(atan2);
		} else if (real instanceof LongnumStructImpl) {
			final double atan2 = StrictMath.atan2(value, ((LongnumStructImpl) real).longValue());
			return DoubleFloatStruct.toLispFloat(atan2);
		} else if (real instanceof SingleFloatStructImpl) {
			final double atan2 = StrictMath.atan2(value, ((FloatStruct) real).floatValue());
			return DoubleFloatStruct.toLispFloat(atan2);
		} else if (real instanceof DoubleFloatStructImpl) {
			final double atan2 = StrictMath.atan2(value, ((FloatStruct) real).doubleValue());
			return DoubleFloatStruct.toLispFloat(atan2);
		}
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
		return IntegerStruct.toLispInteger(Math.abs(value));
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
		if (number instanceof FixnumStructImpl) {
			return value == ((FixnumStructImpl) number).intValue();
		} else if (number instanceof LongnumStructImpl) {
			return value == ((LongnumStructImpl) number).longValue();
		} else if (number instanceof BignumStructImpl) {
			final BigInteger bigInteger1 = BigInteger.valueOf(value);
			final BigInteger bigInteger2 = ((BignumStructImpl) number).bigIntegerValue();
			return bigInteger1.compareTo(bigInteger2) == 0;
		}
		return number.isEqualTo(this);
	}

	@Override
	public boolean isNotEqualTo(final NumberStruct number) {
		if (number instanceof FixnumStructImpl) {
			return value != ((FixnumStructImpl) number).intValue();
		} else if (number instanceof LongnumStructImpl) {
			return value != ((LongnumStructImpl) number).longValue();
		} else if (number instanceof BignumStructImpl) {
			final BigInteger bigInteger1 = BigInteger.valueOf(value);
			final BigInteger bigInteger2 = ((BignumStructImpl) number).bigIntegerValue();
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
			return IntegerStruct.ONE;
		}
		return IntegerStruct.MINUS_ONE;
	}

	@Override
	public IntegerStruct negation() {
		return IntegerStruct.toLispInteger(-value);
	}

	@Override
	public RationalStruct reciprocal() {
		// TODO
		return null;
	}

	@Override
	public RealStruct exp() {
		final double exp = StrictMath.exp(value);
		return DoubleFloatStruct.toLispFloat(exp);
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
		final double log = StrictMath.log(value);
		return DoubleFloatStruct.toLispFloat(log);
	}

	@Override
	public NumberStruct log(final NumberStruct base) {
		// TODO
		final Apint ap = new Apint(value);
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
