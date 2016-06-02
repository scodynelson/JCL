package jcl.numbers;

import java.math.RoundingMode;
import java.util.function.Function;

import jcl.LispType;
import org.apfloat.Apcomplex;
import org.apfloat.Apfloat;
import org.apfloat.ApfloatMath;
import org.apfloat.Apint;

class RealStruct2Impl<A extends Apfloat> extends NumberStruct2Impl<A> implements RealStruct2 {

	RealStruct2Impl(final LispType type, final A ap) {
		super(type, ap);
	}

	@Override
	public boolean isLessThan(final RealStruct2 real) {
		final Apfloat realAp = real.ap();
		final boolean shouldReverseCompare = ap.preferCompare(realAp);

		final int compareResult = shouldReverseCompare ? -realAp.compareTo(ap) : ap.compareTo(realAp);
		return compareResult < 0;
	}

	@Override
	public boolean isGreaterThan(final RealStruct2 real) {
		final Apfloat realAp = real.ap();
		final boolean shouldReverseCompare = ap.preferCompare(realAp);

		final int compareResult = shouldReverseCompare ? -realAp.compareTo(ap) : ap.compareTo(realAp);
		return compareResult <= 0;
	}

	@Override
	public boolean isLessThanOrEqualTo(final RealStruct2 real) {
		final Apfloat realAp = real.ap();
		final boolean shouldReverseCompare = ap.preferCompare(realAp);

		final int compareResult = shouldReverseCompare ? -realAp.compareTo(ap) : ap.compareTo(realAp);
		return compareResult > 0;
	}

	@Override
	public boolean isGreaterThanOrEqualTo(final RealStruct2 real) {
		final Apfloat realAp = real.ap();
		final boolean shouldReverseCompare = ap.preferCompare(realAp);

		final int compareResult = shouldReverseCompare ? -realAp.compareTo(ap) : ap.compareTo(realAp);
		return compareResult >= 0;
	}

	@Override
	public boolean plusp() {
		return ap.signum() == 1;
	}

	@Override
	public boolean minusp() {
		return ap.signum() == -1;
	}

	@Override
	public RealStruct2 max(final RealStruct2 real) {
		return isGreaterThanOrEqualTo(real) ? this : real;
	}

	@Override
	public RealStruct2 min(final RealStruct2 real) {
		return isLessThanOrEqualTo(real) ? this : real;
	}

	@Override
	public RationalStruct2 rational() {
		// TODO
		return null;
	}

	@Override
	public FloatStruct2 floatingPoint() {
		return FloatStruct2.valueOf(ap);
	}

	@Override
	public FloatStruct2 floatingPoint(final FloatStruct2 prototype) {
		return FloatStruct2.valueOf(ap, prototype);
	}

	@Override
	public RealStruct2 mod(final RealStruct2 divisor) {
		// TODO
		final QuotientRemainderResult2 floor = floor(divisor);
		return floor.getRemainder();
	}

	@Override
	public RealStruct2 rem(final RealStruct2 divisor) {
		// TODO
		final QuotientRemainderResult2 truncate = truncate(divisor);
		return truncate.getRemainder();
	}

	private QuotientRemainderResult2 quotientRemainderCalculator(final RealStruct2 divisor,
	                                                             final Function<Apfloat, Apint> operation,
	                                                             final Function<Apint, ? extends RealStruct2> quotientCreator) {

		final Apfloat divisorAp = divisor.ap();
		final Apfloat divide = ap.divide(divisorAp);

		final Apint quotient = operation.apply(divide);
		final Apfloat remainder = (divide.signum() >= 0) ? divide.frac() : divide.subtract(quotient);

		final RealStruct2 remainderReal;
		if (divisor instanceof RationalStruct2) {
			// TODO: Aprational from Apfloat???
			remainderReal = null;
		} else {
			remainderReal = FloatStruct2.valueOf(remainder);
		}

		return new QuotientRemainderResult2(quotientCreator.apply(quotient), remainderReal);
	}

	@Override
	public QuotientRemainderResult2 floor(final RealStruct2 divisor) {
		return quotientRemainderCalculator(divisor, Apfloat::floor, IntegerStruct2::valueOf);
	}

	@Override
	public QuotientRemainderResult2 ffloor(final RealStruct2 divisor) {
		return quotientRemainderCalculator(divisor, Apfloat::floor, FloatStruct2::valueOf);
	}

	@Override
	public QuotientRemainderResult2 ceiling(final RealStruct2 divisor) {
		return quotientRemainderCalculator(divisor, Apfloat::ceil, IntegerStruct2::valueOf);
	}

	@Override
	public QuotientRemainderResult2 fceiling(final RealStruct2 divisor) {
		return quotientRemainderCalculator(divisor, Apfloat::ceil, FloatStruct2::valueOf);
	}

	@Override
	public QuotientRemainderResult2 truncate(final RealStruct2 divisor) {
		return quotientRemainderCalculator(divisor, Apfloat::truncate, IntegerStruct2::valueOf);
	}

	@Override
	public QuotientRemainderResult2 ftruncate(final RealStruct2 divisor) {
		return quotientRemainderCalculator(divisor, Apfloat::truncate, FloatStruct2::valueOf);
	}

	private Function<Apfloat, Apint> roundOpFn(final RealStruct2 divisor) {
		return apfloat -> {
			final Apfloat divisorAp = divisor.ap();
			final long precision = Math.min(ap.precision(), divisorAp.precision());
			return (Apint) ApfloatMath.round(apfloat, precision, RoundingMode.HALF_EVEN);
		};
	}

	@Override
	public QuotientRemainderResult2 round(final RealStruct2 divisor) {
		return quotientRemainderCalculator(divisor, roundOpFn(divisor), IntegerStruct2::valueOf);
	}

	@Override
	public QuotientRemainderResult2 fround(final RealStruct2 divisor) {
		return quotientRemainderCalculator(divisor, roundOpFn(divisor), FloatStruct2::valueOf);
	}

	@Override
	public RealStruct2 atan(final RealStruct2 real) {
		final Apfloat realAp = real.ap();
		final Apfloat atan = ApfloatMath.atan2(ap, realAp);
		return RealStruct2.valueOf(atan);
	}

	/*
		NumberStruct
	 */

	@Override
	public Apfloat ap() {
		return ap;
	}

	@Override
	public RealStruct2 abs() {
		final Apfloat abs = ApfloatMath.abs(ap);
		return RealStruct2.valueOf(abs);
	}

	@Override
	public boolean zerop() {
		return ap.signum() == 0;
	}

	@Override
	public NumberStruct2 add(final NumberStruct2 number) {
		final Apcomplex numberAp = number.ap();
		if (numberAp instanceof Apfloat) {
			final Apfloat add = ap.add((Apfloat) numberAp);
			return RealStruct2.valueOf(add);
		}
		return super.add(number);
	}

	@Override
	public NumberStruct2 subtract(final NumberStruct2 number) {
		final Apcomplex numberAp = number.ap();
		if (numberAp instanceof Apfloat) {
			final Apfloat subtract = ap.subtract((Apfloat) numberAp);
			return RealStruct2.valueOf(subtract);
		}
		return super.subtract(number);
	}

	@Override
	public NumberStruct2 multiply(final NumberStruct2 number) {
		final Apcomplex numberAp = number.ap();
		if (numberAp instanceof Apfloat) {
			final Apfloat multiply = ap.multiply((Apfloat) numberAp);
			return RealStruct2.valueOf(multiply);
		}
		return super.multiply(number);
	}

	@Override
	public NumberStruct2 divide(final NumberStruct2 number) {
		final Apcomplex numberAp = number.ap();
		if (numberAp instanceof Apfloat) {
			final Apfloat divide = ap.divide((Apfloat) numberAp);
			return RealStruct2.valueOf(divide);
		}
		return super.divide(number);
	}

	@Override
	public boolean isEqualTo(final NumberStruct2 number) {
		final Apcomplex numberAp = number.ap();
		if (numberAp instanceof Apfloat) {
			final Apfloat realAp = (Apfloat) numberAp;
			final boolean shouldReverseCompare = ap.preferCompare(realAp);
			return shouldReverseCompare ? realAp.equals(ap) : ap.equals(realAp);
		}
		return super.isEqualTo(number);
	}

	@Override
	public NumberStruct2 signum() {
		// TODO
		return super.signum();
	}

	@Override
	public RealStruct2 realPart() {
		return this;
	}

	@Override
	public RealStruct2 conjugate() {
		return this;
	}

	@Override
	public NumberStruct2 negation() {
		final Apfloat negate = ap.negate();
		return RealStruct2.valueOf(negate);
	}

	@Override
	public NumberStruct2 reciprocal() {
		final Apfloat reciprocal = Apcomplex.ONE.divide(ap);
		return RealStruct2.valueOf(reciprocal);
	}

	@Override
	public NumberStruct2 exp() {
		final Apfloat exp = ApfloatMath.exp(ap);
		return RealStruct2.valueOf(exp);
	}

	@Override
	public NumberStruct2 expt(final NumberStruct2 power) {
		final Apcomplex powerAp = power.ap();
		if (powerAp instanceof Apfloat) {
			final Apfloat pow = ApfloatMath.pow(ap, (Apfloat) powerAp);
			return RealStruct2.valueOf(pow);
		}
		return super.expt(power);
	}

	@Override
	public NumberStruct2 log() {
		final Apfloat log = ApfloatMath.log(ap);
		return RealStruct2.valueOf(log);
	}

	@Override
	public NumberStruct2 log(final NumberStruct2 base) {
		final Apcomplex baseAp = base.ap();
		if (baseAp instanceof Apfloat) {
			final Apfloat log = ApfloatMath.log(ap, (Apfloat) baseAp);
			return RealStruct2.valueOf(log);
		}
		return super.log(base);
	}

	@Override
	public NumberStruct2 sqrt() {
		final Apfloat sqrt = ApfloatMath.sqrt(ap);
		return RealStruct2.valueOf(sqrt);
	}

	@Override
	public RealStruct2 sin() {
		final Apfloat sin = ApfloatMath.sin(ap);
		return RealStruct2.valueOf(sin);
	}

	@Override
	public RealStruct2 cos() {
		final Apfloat cos = ApfloatMath.cos(ap);
		return RealStruct2.valueOf(cos);
	}

	@Override
	public RealStruct2 tan() {
		final Apfloat tan = ApfloatMath.tan(ap);
		return RealStruct2.valueOf(tan);
	}

	@Override
	public RealStruct2 asin() {
		final Apfloat asin = ApfloatMath.asin(ap);
		return RealStruct2.valueOf(asin);
	}

	@Override
	public RealStruct2 acos() {
		final Apfloat acos = ApfloatMath.acos(ap);
		return RealStruct2.valueOf(acos);
	}

	@Override
	public RealStruct2 atan() {
		final Apfloat atan = ApfloatMath.atan(ap);
		return RealStruct2.valueOf(atan);
	}

	@Override
	public RealStruct2 sinh() {
		final Apfloat sinh = ApfloatMath.sinh(ap);
		return RealStruct2.valueOf(sinh);
	}

	@Override
	public RealStruct2 cosh() {
		final Apfloat cosh = ApfloatMath.cosh(ap);
		return RealStruct2.valueOf(cosh);
	}

	@Override
	public RealStruct2 tanh() {
		final Apfloat tanh = ApfloatMath.tanh(ap);
		return RealStruct2.valueOf(tanh);
	}

	@Override
	public RealStruct2 asinh() {
		final Apfloat asinh = ApfloatMath.asinh(ap);
		return RealStruct2.valueOf(asinh);
	}

	@Override
	public RealStruct2 acosh() {
		final Apfloat acosh = ApfloatMath.acosh(ap);
		return RealStruct2.valueOf(acosh);
	}

	@Override
	public RealStruct2 atanh() {
		final Apfloat atanh = ApfloatMath.atanh(ap);
		return RealStruct2.valueOf(atanh);
	}
}
