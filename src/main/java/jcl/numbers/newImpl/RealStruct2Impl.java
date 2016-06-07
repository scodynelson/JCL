/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers.newImpl;

import java.math.RoundingMode;
import java.util.function.Function;

import jcl.LispType;
import org.apfloat.Apcomplex;
import org.apfloat.Apfloat;
import org.apfloat.ApfloatMath;
import org.apfloat.Apint;

/**
 * Internal implementation class for {@link RealStruct2} objects.
 *
 * @param <A>
 * 		the type of {@link Apfloat} the {@link RealStruct2} object will use for its value
 */
abstract class RealStruct2Impl<A extends Apfloat> extends NumberStruct2Impl<A> implements RealStruct2 {

	/**
	 * Package level constructor that passes the provided {@link LispType} and {@link A} {@link Apfloat} value to the
	 * {@link NumberStruct2Impl} superclass constructor.
	 *
	 * @param type
	 * 		the {@link LispType} of the {@link RealStruct2}
	 * @param ap
	 * 		the internal {@link Apfloat} implementation value of the {@link RealStruct2}
	 */
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

	/**
	 * Returns the {@link RealStruct2} that represents the {@link QuotientRemainder2#remainder} value based on {@code
	 * this} class and the class of the provided {@code divisor}.
	 *
	 * @param divisor
	 * 		the {@link RealStruct2} used as the divisor in the quotient-remainder operation, and used in determining the
	 * 		resulting {@link RealStruct2} instance type
	 * @param remainder
	 * 		the implementation value of the remainder
	 *
	 * @return the {@link RealStruct2} that represents the appropriate {@link QuotientRemainder2#remainder} value
	 */
	protected RealStruct2 getRemainderReal(final RealStruct2 divisor, final Apfloat remainder) {
		return FloatStruct2.valueOf(remainder);
	}

	/**
	 * Calculates the resulting {@link QuotientRemainder2} from the provided {@link Apfloat} number and {@link
	 * RealStruct2} divisor, using the {@link Function} operation as the quotient-remainder operation to perform and
	 * the {@link Function} quotientCreator for the creation of the {@link QuotientRemainder2#quotient} value. The
	 * {@link QuotientRemainder2#remainder} value is retrieved via the possibly overridden {@link
	 * #getRemainderReal(RealStruct2, Apfloat)} method.
	 *
	 * @param number
	 * 		the implementation value to perform the quotient-remainder operation on
	 * @param divisor
	 * 		the divisor used in calculating the {@code number} parameter
	 * @param operation
	 * 		the quotient-remainder operation to perform on the {@code number} parameter
	 * @param quotientCreator
	 * 		the {@link Function} to be used for the creation of the {@link QuotientRemainder2#quotient} value
	 *
	 * @return the resulting {@link QuotientRemainder2} calculated from the quotient-remainder operation and the
	 * appropriately calculated remainder value
	 */
	private QuotientRemainder2 quotientRemainderCalculator(final Apfloat number, final RealStruct2 divisor,
	                                                       final Function<Apfloat, Apint> operation,
	                                                       final Function<Apint, ? extends RealStruct2> quotientCreator) {
		final Apint quotient = operation.apply(number);
		final Apfloat remainder = (number.signum() >= 0) ? number.frac() : number.subtract(quotient);

		final RealStruct2 quotientReal = quotientCreator.apply(quotient);
		final RealStruct2 remainderReal = getRemainderReal(divisor, remainder);

		return new QuotientRemainder2(quotientReal, remainderReal);
	}

	/**
	 * Performs the default quotient-remainder calculation against the {@link #ap} value, using {@link
	 * IntegerStruct2#ONE} as the divisor. The {@code operation} and {@code quotientCreator} parameters are further
	 * passed to the {@link #quotientRemainderCalculator(Apfloat, RealStruct2, Function, Function)} method.
	 *
	 * @param operation
	 * 		the quotient-remainder operation to perform on the {@link #ap} value
	 * @param quotientCreator
	 * 		the {@link Function} to be used for the creation of the {@link QuotientRemainder2#quotient} value
	 *
	 * @return the resulting {@link QuotientRemainder2} calculated from the quotient-remainder operation
	 */
	private QuotientRemainder2 quotientRemainder(final Function<Apfloat, Apint> operation,
	                                             final Function<Apint, ? extends RealStruct2> quotientCreator) {
		return quotientRemainderCalculator(ap, IntegerStruct2.ONE, operation, quotientCreator);
	}

	/**
	 * Performs the default quotient-remainder calculation against the result of dividing {@link #ap} by the divisor
	 * parameter. The {@code divisor}, {@code operation}, and {@code quotientCreator} parameters are further passed to
	 * the {@link #quotientRemainderCalculator(Apfloat, RealStruct2, Function, Function)} method.
	 *
	 * @param divisor
	 * 		the divisor used in calculating the {@code number} parameter
	 * @param operation
	 * 		the quotient-remainder operation to perform on the calculated number from dividing {@link #ap} by the divisor
	 * 		parameter
	 * @param quotientCreator
	 * 		the {@link Function} to be used for the creation of the {@link QuotientRemainder2#quotient} value
	 *
	 * @return the resulting {@link QuotientRemainder2} calculated from the quotient-remainder operation
	 */
	private QuotientRemainder2 quotientRemainderWithDivisor(final RealStruct2 divisor,
	                                                        final Function<Apfloat, Apint> operation,
	                                                        final Function<Apint, ? extends RealStruct2> quotientCreator) {
		final Apfloat divisorAp = divisor.ap();
		final Apfloat number = ap.divide(divisorAp);
		return quotientRemainderCalculator(number, divisor, operation, quotientCreator);
	}

	@Override
	public QuotientRemainder2 floor() {
		return quotientRemainder(Apfloat::floor, IntegerStruct2::valueOf);
	}

	@Override
	public QuotientRemainder2 floor(final RealStruct2 divisor) {
		return quotientRemainderWithDivisor(divisor, Apfloat::floor, IntegerStruct2::valueOf);
	}

	@Override
	public QuotientRemainder2 ffloor() {
		return quotientRemainder(Apfloat::floor, FloatStruct2::valueOf);
	}

	@Override
	public QuotientRemainder2 ffloor(final RealStruct2 divisor) {
		return quotientRemainderWithDivisor(divisor, Apfloat::floor, FloatStruct2::valueOf);
	}

	@Override
	public QuotientRemainder2 ceiling() {
		return quotientRemainder(Apfloat::ceil, IntegerStruct2::valueOf);
	}

	@Override
	public QuotientRemainder2 ceiling(final RealStruct2 divisor) {
		return quotientRemainderWithDivisor(divisor, Apfloat::ceil, IntegerStruct2::valueOf);
	}

	@Override
	public QuotientRemainder2 fceiling() {
		return quotientRemainder(Apfloat::ceil, FloatStruct2::valueOf);
	}

	@Override
	public QuotientRemainder2 fceiling(final RealStruct2 divisor) {
		return quotientRemainderWithDivisor(divisor, Apfloat::ceil, FloatStruct2::valueOf);
	}

	@Override
	public QuotientRemainder2 truncate() {
		return quotientRemainder(Apfloat::truncate, IntegerStruct2::valueOf);
	}

	@Override
	public QuotientRemainder2 truncate(final RealStruct2 divisor) {
		return quotientRemainderWithDivisor(divisor, Apfloat::truncate, IntegerStruct2::valueOf);
	}

	@Override
	public QuotientRemainder2 ftruncate() {
		return quotientRemainder(Apfloat::truncate, FloatStruct2::valueOf);
	}

	@Override
	public QuotientRemainder2 ftruncate(final RealStruct2 divisor) {
		return quotientRemainderWithDivisor(divisor, Apfloat::truncate, FloatStruct2::valueOf);
	}

	/**
	 * Creates the {@link Function} operation to be used for the ROUND and FROUND quotient-remainder calculations. This
	 * rounding method uses {@link RoundingMode#HALF_EVEN} and rounds according to the least precise of the {@link
	 * Apfloat#precision()} of {@link #ap} and the {@link RealStruct2#ap()} of the provided divisor.
	 *
	 * @param divisor
	 * 		the divisor used in determining the rounding precision to be used
	 *
	 * @return the {@link Function} operation to be used for the ROUND and FROUND quotient-remainder calculation
	 */
	private Function<Apfloat, Apint> roundOpFn(final RealStruct2 divisor) {
		return apfloat -> {
			final Apfloat divisorAp = divisor.ap();
			final long precision = Math.min(ap.precision(), divisorAp.precision());
			return (Apint) ApfloatMath.round(apfloat, precision, RoundingMode.HALF_EVEN);
		};
	}

	@Override
	public QuotientRemainder2 round() {
		return quotientRemainder(roundOpFn(IntegerStruct2.ONE), IntegerStruct2::valueOf);
	}

	@Override
	public QuotientRemainder2 round(final RealStruct2 divisor) {
		return quotientRemainderWithDivisor(divisor, roundOpFn(divisor), IntegerStruct2::valueOf);
	}

	@Override
	public QuotientRemainder2 fround() {
		return quotientRemainder(roundOpFn(IntegerStruct2.ONE), FloatStruct2::valueOf);
	}

	@Override
	public QuotientRemainder2 fround(final RealStruct2 divisor) {
		return quotientRemainderWithDivisor(divisor, roundOpFn(divisor), FloatStruct2::valueOf);
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
	public abstract RealStruct2 signum();

	@Override
	public RealStruct2 realPart() {
		return this;
	}

	@Override
	public RealStruct2 conjugate() {
		return this;
	}

	@Override
	public RealStruct2 negation() {
		final Apfloat negate = ap.negate();
		return RealStruct2.valueOf(negate);
	}

	@Override
	public RealStruct2 reciprocal() {
		final Apfloat reciprocal = Apcomplex.ONE.divide(ap);
		return RealStruct2.valueOf(reciprocal);
	}

	@Override
	public RealStruct2 exp() {
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
	public RealStruct2 log() {
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
	public RealStruct2 sqrt() {
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
