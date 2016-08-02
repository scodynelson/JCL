/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.internal.number;

import java.math.RoundingMode;
import java.util.function.Function;

import jcl.lang.IntegerStruct;
import jcl.lang.NumberStruct;
import jcl.lang.RealStruct;
import jcl.lang.number.QuotientRemainder;
import jcl.type.LispType;
import org.apfloat.Apcomplex;
import org.apfloat.Apfloat;
import org.apfloat.ApfloatMath;
import org.apfloat.Apint;

/**
 * Internal implementation class for {@link RealStruct} objects.
 *
 * @param <A>
 * 		the type of {@link Apfloat} the {@link RealStruct} object will use for its value
 */
abstract class RealStructImpl<A extends Apfloat> extends NumberStructImpl<A> implements RealStruct {

	/**
	 * Package level constructor that passes the provided {@link LispType} and {@link A} {@link Apfloat} value to the
	 * {@link NumberStructImpl} superclass constructor.
	 *
	 * @param type
	 * 		the {@link LispType} of the {@link RealStruct}
	 * @param ap
	 * 		the internal {@link Apfloat} implementation value of the {@link RealStruct}
	 */
	RealStructImpl(final LispType type, final A ap) {
		super(type, ap);
	}

	@Override
	public boolean isLessThan(final RealStruct real) {
		final Apfloat realAp = real.ap();
		final boolean shouldReverseCompare = ap.preferCompare(realAp);

		final int compareResult = shouldReverseCompare ? -realAp.compareTo(ap) : ap.compareTo(realAp);
		return compareResult < 0;
	}

	@Override
	public boolean isGreaterThan(final RealStruct real) {
		final Apfloat realAp = real.ap();
		final boolean shouldReverseCompare = ap.preferCompare(realAp);

		final int compareResult = shouldReverseCompare ? -realAp.compareTo(ap) : ap.compareTo(realAp);
		return compareResult <= 0;
	}

	@Override
	public boolean isLessThanOrEqualTo(final RealStruct real) {
		final Apfloat realAp = real.ap();
		final boolean shouldReverseCompare = ap.preferCompare(realAp);

		final int compareResult = shouldReverseCompare ? -realAp.compareTo(ap) : ap.compareTo(realAp);
		return compareResult > 0;
	}

	@Override
	public boolean isGreaterThanOrEqualTo(final RealStruct real) {
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
	 * Returns the {@link RealStruct} that represents the {@link QuotientRemainder#remainder} value based on {@code
	 * this} class and the class of the provided {@code divisor}.
	 *
	 * @param divisor
	 * 		the {@link RealStruct} used as the divisor in the quotient-remainder operation, and used in determining the
	 * 		resulting {@link RealStruct} instance type
	 * @param remainder
	 * 		the implementation value of the remainder
	 *
	 * @return the {@link RealStruct} that represents the appropriate {@link QuotientRemainder#remainder} value
	 */
	protected RealStruct getRemainderReal(final RealStruct divisor, final Apfloat remainder) {
		return FloatStructImpl.valueOf(remainder);
	}

	/**
	 * Calculates the resulting {@link QuotientRemainder} from the provided {@link Apfloat} number and {@link
	 * RealStruct} divisor, using the {@link Function} operation as the quotient-remainder operation to perform and
	 * the {@link Function} quotientCreator for the creation of the {@link QuotientRemainder#quotient} value. The
	 * {@link QuotientRemainder#remainder} value is retrieved via the possibly overridden {@link
	 * #getRemainderReal(RealStruct, Apfloat)} method.
	 *
	 * @param number
	 * 		the implementation value to perform the quotient-remainder operation on
	 * @param divisor
	 * 		the divisor used in calculating the {@code number} parameter
	 * @param operation
	 * 		the quotient-remainder operation to perform on the {@code number} parameter
	 * @param quotientCreator
	 * 		the {@link Function} to be used for the creation of the {@link QuotientRemainder#quotient} value
	 *
	 * @return the resulting {@link QuotientRemainder} calculated from the quotient-remainder operation and the
	 * appropriately calculated remainder value
	 */
	private QuotientRemainder quotientRemainderCalculator(final Apfloat number, final RealStruct divisor,
	                                                      final Function<Apfloat, Apint> operation,
	                                                      final Function<Apint, ? extends RealStruct> quotientCreator) {
		final Apint quotient = operation.apply(number);
		final Apfloat remainder = (number.signum() >= 0) ? number.frac() : number.subtract(quotient);

		final RealStruct quotientReal = quotientCreator.apply(quotient);
		final RealStruct remainderReal = getRemainderReal(divisor, remainder);

		return new QuotientRemainder(quotientReal, remainderReal);
	}

	/**
	 * Performs the default quotient-remainder calculation against the {@link #ap} value, using {@link
	 * IntegerStruct#ONE} as the divisor. The {@code operation} and {@code quotientCreator} parameters are further
	 * passed to the {@link #quotientRemainderCalculator(Apfloat, RealStruct, Function, Function)} method.
	 *
	 * @param operation
	 * 		the quotient-remainder operation to perform on the {@link #ap} value
	 * @param quotientCreator
	 * 		the {@link Function} to be used for the creation of the {@link QuotientRemainder#quotient} value
	 *
	 * @return the resulting {@link QuotientRemainder} calculated from the quotient-remainder operation
	 */
	private QuotientRemainder quotientRemainder(final Function<Apfloat, Apint> operation,
	                                            final Function<Apint, ? extends RealStruct> quotientCreator) {
		return quotientRemainderCalculator(ap, IntegerStruct.ONE, operation, quotientCreator);
	}

	/**
	 * Performs the default quotient-remainder calculation against the result of dividing {@link #ap} by the divisor
	 * parameter. The {@code divisor}, {@code operation}, and {@code quotientCreator} parameters are further passed to
	 * the {@link #quotientRemainderCalculator(Apfloat, RealStruct, Function, Function)} method.
	 *
	 * @param divisor
	 * 		the divisor used in calculating the {@code number} parameter
	 * @param operation
	 * 		the quotient-remainder operation to perform on the calculated number from dividing {@link #ap} by the divisor
	 * 		parameter
	 * @param quotientCreator
	 * 		the {@link Function} to be used for the creation of the {@link QuotientRemainder#quotient} value
	 *
	 * @return the resulting {@link QuotientRemainder} calculated from the quotient-remainder operation
	 */
	private QuotientRemainder quotientRemainderWithDivisor(final RealStruct divisor,
	                                                       final Function<Apfloat, Apint> operation,
	                                                       final Function<Apint, ? extends RealStruct> quotientCreator) {
		final Apfloat divisorAp = divisor.ap();
		final Apfloat number = ap.divide(divisorAp);
		return quotientRemainderCalculator(number, divisor, operation, quotientCreator);
	}

	@Override
	public QuotientRemainder floor() {
		return quotientRemainder(Apfloat::floor, IntegerStructImpl::valueOf);
	}

	@Override
	public QuotientRemainder floor(final RealStruct divisor) {
		return quotientRemainderWithDivisor(divisor, Apfloat::floor, IntegerStructImpl::valueOf);
	}

	@Override
	public QuotientRemainder ffloor() {
		return quotientRemainder(Apfloat::floor, FloatStructImpl::valueOf);
	}

	@Override
	public QuotientRemainder ffloor(final RealStruct divisor) {
		return quotientRemainderWithDivisor(divisor, Apfloat::floor, FloatStructImpl::valueOf);
	}

	@Override
	public QuotientRemainder ceiling() {
		return quotientRemainder(Apfloat::ceil, IntegerStructImpl::valueOf);
	}

	@Override
	public QuotientRemainder ceiling(final RealStruct divisor) {
		return quotientRemainderWithDivisor(divisor, Apfloat::ceil, IntegerStructImpl::valueOf);
	}

	@Override
	public QuotientRemainder fceiling() {
		return quotientRemainder(Apfloat::ceil, FloatStructImpl::valueOf);
	}

	@Override
	public QuotientRemainder fceiling(final RealStruct divisor) {
		return quotientRemainderWithDivisor(divisor, Apfloat::ceil, FloatStructImpl::valueOf);
	}

	@Override
	public QuotientRemainder truncate() {
		return quotientRemainder(Apfloat::truncate, IntegerStructImpl::valueOf);
	}

	@Override
	public QuotientRemainder truncate(final RealStruct divisor) {
		return quotientRemainderWithDivisor(divisor, Apfloat::truncate, IntegerStructImpl::valueOf);
	}

	@Override
	public QuotientRemainder ftruncate() {
		return quotientRemainder(Apfloat::truncate, FloatStructImpl::valueOf);
	}

	@Override
	public QuotientRemainder ftruncate(final RealStruct divisor) {
		return quotientRemainderWithDivisor(divisor, Apfloat::truncate, FloatStructImpl::valueOf);
	}

	/**
	 * Creates the {@link Function} operation to be used for the ROUND and FROUND quotient-remainder calculations. This
	 * rounding method uses {@link RoundingMode#HALF_EVEN} and rounds according to the least precise of the {@link
	 * Apfloat#precision()} of {@link #ap} and the {@link RealStruct#ap()} of the provided divisor.
	 *
	 * @param divisor
	 * 		the divisor used in determining the rounding precision to be used
	 *
	 * @return the {@link Function} operation to be used for the ROUND and FROUND quotient-remainder calculation
	 */
	private Function<Apfloat, Apint> roundOpFn(final RealStruct divisor) {
		return apfloat -> {
			final Apfloat divisorAp = divisor.ap();
			final long precision = Math.min(ap.precision(), divisorAp.precision());
			return (Apint) ApfloatMath.round(apfloat, precision, RoundingMode.HALF_EVEN);
		};
	}

	@Override
	public QuotientRemainder round() {
		return quotientRemainder(roundOpFn(IntegerStruct.ONE), IntegerStructImpl::valueOf);
	}

	@Override
	public QuotientRemainder round(final RealStruct divisor) {
		return quotientRemainderWithDivisor(divisor, roundOpFn(divisor), IntegerStructImpl::valueOf);
	}

	@Override
	public QuotientRemainder fround() {
		return quotientRemainder(roundOpFn(IntegerStruct.ONE), FloatStructImpl::valueOf);
	}

	@Override
	public QuotientRemainder fround(final RealStruct divisor) {
		return quotientRemainderWithDivisor(divisor, roundOpFn(divisor), FloatStructImpl::valueOf);
	}

	@Override
	public RealStruct atan(final RealStruct real) {
		final Apfloat realAp = real.ap();
		final Apfloat atan = ApfloatMath.atan2(ap, realAp);
		return RealStruct.valueOf(atan);
	}

	/*
		NumberStruct
	 */

	@Override
	public Apfloat ap() {
		return ap;
	}

	@Override
	public RealStruct abs() {
		final Apfloat abs = ApfloatMath.abs(ap);
		return RealStruct.valueOf(abs);
	}

	@Override
	public boolean zerop() {
		return ap.signum() == 0;
	}

	@Override
	public NumberStruct add(final NumberStruct number) {
		final Apcomplex numberAp = number.ap();
		if (numberAp instanceof Apfloat) {
			final Apfloat add = ap.add((Apfloat) numberAp);
			return RealStruct.valueOf(add);
		}
		return super.add(number);
	}

	@Override
	public NumberStruct subtract(final NumberStruct number) {
		final Apcomplex numberAp = number.ap();
		if (numberAp instanceof Apfloat) {
			final Apfloat subtract = ap.subtract((Apfloat) numberAp);
			return RealStruct.valueOf(subtract);
		}
		return super.subtract(number);
	}

	@Override
	public NumberStruct multiply(final NumberStruct number) {
		final Apcomplex numberAp = number.ap();
		if (numberAp instanceof Apfloat) {
			final Apfloat multiply = ap.multiply((Apfloat) numberAp);
			return RealStruct.valueOf(multiply);
		}
		return super.multiply(number);
	}

	@Override
	public NumberStruct divide(final NumberStruct number) {
		final Apcomplex numberAp = number.ap();
		if (numberAp instanceof Apfloat) {
			final Apfloat divide = ap.divide((Apfloat) numberAp);
			return RealStruct.valueOf(divide);
		}
		return super.divide(number);
	}

	@Override
	public boolean isEqualTo(final NumberStruct number) {
		final Apcomplex numberAp = number.ap();
		if (numberAp instanceof Apfloat) {
			final Apfloat realAp = (Apfloat) numberAp;
			final boolean shouldReverseCompare = ap.preferCompare(realAp);
			return shouldReverseCompare ? realAp.equals(ap) : ap.equals(realAp);
		}
		return super.isEqualTo(number);
	}

	@Override
	public abstract RealStruct signum();

	@Override
	public RealStruct realPart() {
		return this;
	}

	@Override
	public RealStruct conjugate() {
		return this;
	}

	@Override
	public RealStruct negation() {
		final Apfloat negate = ap.negate();
		return RealStruct.valueOf(negate);
	}

	@Override
	public RealStruct reciprocal() {
		final Apfloat reciprocal = Apcomplex.ONE.divide(ap);
		return RealStruct.valueOf(reciprocal);
	}

	@Override
	public RealStruct exp() {
		final Apfloat exp = ApfloatMath.exp(ap);
		return RealStruct.valueOf(exp);
	}

	@Override
	public NumberStruct expt(final NumberStruct power) {
		final Apcomplex powerAp = power.ap();
		if (powerAp instanceof Apfloat) {
			final Apfloat pow = ApfloatMath.pow(ap, (Apfloat) powerAp);
			return RealStruct.valueOf(pow);
		}
		return super.expt(power);
	}

	@Override
	public RealStruct log() {
		final Apfloat log = ApfloatMath.log(ap);
		return RealStruct.valueOf(log);
	}

	@Override
	public NumberStruct log(final NumberStruct base) {
		final Apcomplex baseAp = base.ap();
		if (baseAp instanceof Apfloat) {
			final Apfloat log = ApfloatMath.log(ap, (Apfloat) baseAp);
			return RealStruct.valueOf(log);
		}
		return super.log(base);
	}

	@Override
	public RealStruct sqrt() {
		final Apfloat sqrt = ApfloatMath.sqrt(ap);
		return RealStruct.valueOf(sqrt);
	}

	@Override
	public RealStruct sin() {
		final Apfloat sin = ApfloatMath.sin(ap);
		return RealStruct.valueOf(sin);
	}

	@Override
	public RealStruct cos() {
		final Apfloat cos = ApfloatMath.cos(ap);
		return RealStruct.valueOf(cos);
	}

	@Override
	public RealStruct tan() {
		final Apfloat tan = ApfloatMath.tan(ap);
		return RealStruct.valueOf(tan);
	}

	@Override
	public RealStruct asin() {
		final Apfloat asin = ApfloatMath.asin(ap);
		return RealStruct.valueOf(asin);
	}

	@Override
	public RealStruct acos() {
		final Apfloat acos = ApfloatMath.acos(ap);
		return RealStruct.valueOf(acos);
	}

	@Override
	public RealStruct atan() {
		final Apfloat atan = ApfloatMath.atan(ap);
		return RealStruct.valueOf(atan);
	}

	@Override
	public RealStruct sinh() {
		final Apfloat sinh = ApfloatMath.sinh(ap);
		return RealStruct.valueOf(sinh);
	}

	@Override
	public RealStruct cosh() {
		final Apfloat cosh = ApfloatMath.cosh(ap);
		return RealStruct.valueOf(cosh);
	}

	@Override
	public RealStruct tanh() {
		final Apfloat tanh = ApfloatMath.tanh(ap);
		return RealStruct.valueOf(tanh);
	}

	@Override
	public RealStruct asinh() {
		final Apfloat asinh = ApfloatMath.asinh(ap);
		return RealStruct.valueOf(asinh);
	}

	@Override
	public RealStruct acosh() {
		final Apfloat acosh = ApfloatMath.acosh(ap);
		return RealStruct.valueOf(acosh);
	}

	@Override
	public RealStruct atanh() {
		final Apfloat atanh = ApfloatMath.atanh(ap);
		return RealStruct.valueOf(atanh);
	}
}
