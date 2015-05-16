/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.math.RoundingMode;
import java.util.List;

import jcl.LispStruct;
import jcl.types.RealType;
import org.apache.commons.math3.util.FastMath;

/**
 * The {@link RealStruct} is the object representation of a Lisp 'real' type.
 */
public abstract class RealStruct extends NumberStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -7159935653316309907L;

	/**
	 * Protected constructor.
	 *
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected RealStruct(final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		super(RealType.INSTANCE, directSuperClasses, subClasses);
	}

	/**
	 * Protected constructor.
	 *
	 * @param type
	 * 		the type of the real object
	 * @param directSuperClasses
	 * 		the direct super classes
	 * @param subClasses
	 * 		the subclasses
	 */
	protected RealStruct(final RealType type,
	                     final List<Class<? extends LispStruct>> directSuperClasses, final List<Class<? extends LispStruct>> subClasses) {
		super(type, directSuperClasses, subClasses);
	}

	@Override
	public NumberStruct log(final NumberStruct base) {
		if (base instanceof RealStruct) {
			final double number = doubleValue();
			final double baseVal = ((RealStruct) base).doubleValue();
			final double log = FastMath.log(baseVal, number);
			return new FloatStruct(log);
		}
		return super.log(base);
	}

	public abstract BigDecimal bigDecimalValue();

	public abstract double doubleValue();

	public abstract boolean plusp();

	public abstract boolean minusp();

	@Override
	public RealStruct realPart() {
		return this;
	}

	public RealStruct mod(final RealStruct divisor) {
		final QuotientRemainderResult floor = floor(divisor);
		return floor.getRemainder();
	}

	public RealStruct rem(final RealStruct divisor) {
		final QuotientRemainderResult truncate = truncate(divisor);
		return truncate.getRemainder();
	}

	public abstract boolean isLessThan(LispStruct obj);

	public abstract boolean isGreaterThan(LispStruct obj);

	public abstract boolean isLessThanOrEqualTo(LispStruct obj);

	public abstract boolean isGreaterThanOrEqualTo(LispStruct obj);

	public abstract RealStruct rational();

	public abstract RealStruct max(RealStruct real);

	public abstract RealStruct min(RealStruct real);

	public ComplexStruct cis() {
		return new ComplexStruct(cos(), sin());
	}

	@Override
	public RealStruct conjugate() {
		return this;
	}

	@Override
	public RealStruct exp() {
		final double doubleValue = doubleValue();
		final double exp = FastMath.exp(doubleValue);
		return new FloatStruct(exp);
	}

	@Override
	public RealStruct sqrt() {
		final double doubleValue = doubleValue();
		final double sqrt = FastMath.sqrt(doubleValue);
		return new FloatStruct(sqrt);
	}

	@Override
	public RealStruct log() {
		final double doubleValue = doubleValue();
		final double log = FastMath.log(doubleValue);
		return new FloatStruct(log);
	}

	@Override
	public RealStruct sin() {
		final double doubleValue = doubleValue();
		final double sin = FastMath.sin(doubleValue);
		return new FloatStruct(sin);
	}

	@Override
	public RealStruct cos() {
		final double doubleValue = doubleValue();
		final double cos = FastMath.cos(doubleValue);
		return new FloatStruct(cos);
	}

	@Override
	public RealStruct tan() {
		final double doubleValue = doubleValue();
		final double tan = FastMath.tan(doubleValue);
		return new FloatStruct(tan);
	}

	@Override
	public RealStruct asin() {
		final double doubleValue = doubleValue();
		final double asin = FastMath.asin(doubleValue);
		return new FloatStruct(asin);
	}

	@Override
	public RealStruct acos() {
		final double doubleValue = doubleValue();
		final double acos = FastMath.acos(doubleValue);
		return new FloatStruct(acos);
	}

	@Override
	public RealStruct atan() {
		final double doubleValue = doubleValue();
		final double atan = FastMath.atan(doubleValue);
		return new FloatStruct(atan);
	}

	public RealStruct atan(final RealStruct real) {
		final double doubleValue = doubleValue();
		final double doubleValue2 = real.doubleValue();

		final double atan = FastMath.atan2(doubleValue, doubleValue2);
		return new FloatStruct(atan);
	}

	@Override
	public RealStruct sinh() {
		final double doubleValue = doubleValue();
		final double sinh = FastMath.sinh(doubleValue);
		return new FloatStruct(sinh);
	}

	@Override
	public RealStruct cosh() {
		final double doubleValue = doubleValue();
		final double cosh = FastMath.cosh(doubleValue);
		return new FloatStruct(cosh);
	}

	@Override
	public RealStruct tanh() {
		final double doubleValue = doubleValue();
		final double tanh = FastMath.tanh(doubleValue);
		return new FloatStruct(tanh);
	}

	@Override
	public RealStruct asinh() {
		final double doubleValue = doubleValue();
		final double asinh = FastMath.asinh(doubleValue);
		return new FloatStruct(asinh);
	}

	@Override
	public RealStruct acosh() {
		final double doubleValue = doubleValue();
		final double acosh = FastMath.acosh(doubleValue);
		return new FloatStruct(acosh);
	}

	@Override
	public RealStruct atanh() {
		final double doubleValue = doubleValue();
		final double atanh = FastMath.atanh(doubleValue);
		return new FloatStruct(atanh);
	}

	public QuotientRemainderResult floor() {
		return truncate(IntegerStruct.ONE);
	}

	public QuotientRemainderResult floor(final RealStruct divisor) {

		final BigDecimal numberBigDecimal = bigDecimalValue();
		final BigDecimal quotient = numberBigDecimal.setScale(0, RoundingMode.FLOOR);

		final BigDecimal divisorBigDecimal = divisor.bigDecimalValue();
		final BigDecimal remainder = numberBigDecimal.remainder(divisorBigDecimal, MathContext.DECIMAL128);

		final BigInteger quotientBigInteger = quotient.toBigInteger();
		final RealStruct quotientInteger = new IntegerStruct(quotientBigInteger);
		return new QuotientRemainderResult(quotientInteger, new FloatStruct(remainder));
	}

	public QuotientRemainderResult ffloor() {
		return ftruncate(IntegerStruct.ONE);
	}

	public QuotientRemainderResult ffloor(final RealStruct divisor) {

		final BigDecimal numberBigDecimal = bigDecimalValue();
		final BigDecimal quotient = numberBigDecimal.setScale(0, RoundingMode.FLOOR);

		final BigDecimal divisorBigDecimal = divisor.bigDecimalValue();
		final BigDecimal remainder = numberBigDecimal.remainder(divisorBigDecimal, MathContext.DECIMAL128);

		final RealStruct floatQuotient = getFloatQuotient(divisor, quotient);
		return new QuotientRemainderResult(floatQuotient, new FloatStruct(remainder));
	}

	public QuotientRemainderResult ceiling() {
		return truncate(IntegerStruct.ONE);
	}

	public QuotientRemainderResult ceiling(final RealStruct divisor) {
		final BigDecimal numberBigDecimal = bigDecimalValue();
		final BigDecimal quotient = numberBigDecimal.setScale(0, RoundingMode.CEILING);

		final BigDecimal divisorBigDecimal = divisor.bigDecimalValue();
		final BigDecimal remainder = numberBigDecimal.remainder(divisorBigDecimal, MathContext.DECIMAL128);

		final BigInteger quotientBigInteger = quotient.toBigInteger();
		final RealStruct quotientInteger = new IntegerStruct(quotientBigInteger);
		return new QuotientRemainderResult(quotientInteger, new FloatStruct(remainder));
	}

	public QuotientRemainderResult fceiling() {
		return ftruncate(IntegerStruct.ONE);
	}

	public QuotientRemainderResult fceiling(final RealStruct divisor) {
		final BigDecimal numberBigDecimal = bigDecimalValue();
		final BigDecimal quotient = numberBigDecimal.setScale(0, RoundingMode.CEILING);

		final BigDecimal divisorBigDecimal = divisor.bigDecimalValue();
		final BigDecimal remainder = numberBigDecimal.remainder(divisorBigDecimal, MathContext.DECIMAL128);

		final RealStruct floatQuotient = getFloatQuotient(divisor, quotient);
		return new QuotientRemainderResult(floatQuotient, new FloatStruct(remainder));
	}

	public QuotientRemainderResult truncate() {
		return truncate(IntegerStruct.ONE);
	}

	public QuotientRemainderResult truncate(final RealStruct divisor) {
		if (plusp()) {
			return floor(divisor);
		} else {
			return ceiling(divisor);
		}
	}

	public QuotientRemainderResult ftruncate() {
		return ftruncate(IntegerStruct.ONE);
	}

	public QuotientRemainderResult ftruncate(final RealStruct divisor) {
		if (plusp()) {
			return ffloor(divisor);
		} else {
			return fceiling(divisor);
		}
	}

	public QuotientRemainderResult round() {
		return truncate(IntegerStruct.ONE);
	}

	public QuotientRemainderResult round(final RealStruct divisor) {
		final BigDecimal numberBigDecimal = bigDecimalValue();
		final BigDecimal quotient = numberBigDecimal.setScale(0, RoundingMode.HALF_EVEN);

		final BigDecimal divisorBigDecimal = divisor.bigDecimalValue();
		final BigDecimal remainder = numberBigDecimal.remainder(divisorBigDecimal, MathContext.DECIMAL128);

		final BigInteger quotientBigInteger = quotient.toBigInteger();
		final RealStruct quotientInteger = new IntegerStruct(quotientBigInteger);
		return new QuotientRemainderResult(quotientInteger, new FloatStruct(remainder));
	}

	public QuotientRemainderResult fround() {
		return ftruncate(IntegerStruct.ONE);
	}

	public QuotientRemainderResult fround(final RealStruct divisor) {
		final BigDecimal numberBigDecimal = bigDecimalValue();
		final BigDecimal quotient = numberBigDecimal.setScale(0, RoundingMode.HALF_EVEN);

		final BigDecimal divisorBigDecimal = divisor.bigDecimalValue();
		final BigDecimal remainder = numberBigDecimal.remainder(divisorBigDecimal, MathContext.DECIMAL128);

		final RealStruct floatQuotient = getFloatQuotient(divisor, quotient);
		return new QuotientRemainderResult(floatQuotient, new FloatStruct(remainder));
	}

	private RealStruct getFloatQuotient(final RealStruct divisor, final BigDecimal quotient) {
		final RealStruct floatQuotient;
		if (BigDecimal.ZERO.compareTo(quotient) == 0) {
			if (minusp()) {
				if (divisor.minusp()) {
					floatQuotient = FloatStruct.ZERO;
				} else {
					floatQuotient = FloatStruct.MINUS_ZERO;
				}
			} else if (divisor.minusp()) {
				floatQuotient = FloatStruct.MINUS_ZERO;
			} else {
				floatQuotient = FloatStruct.ZERO;
			}
		} else {
			floatQuotient = new FloatStruct(quotient);
		}
		return floatQuotient;
	}
}
