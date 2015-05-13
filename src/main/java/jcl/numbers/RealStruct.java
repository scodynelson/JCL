/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
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
			return new FloatStruct(new BigDecimal(log));
		}
		return super.log(base);
	}

	public abstract BigDecimal bigDecimalValue();

	public abstract double doubleValue();

	public abstract boolean plusp();

	public abstract boolean minusp();

	public abstract boolean isLessThan(LispStruct obj);

	public abstract boolean isGreaterThan(LispStruct obj);

	public abstract boolean isLessThanOrEqualTo(LispStruct obj);

	public abstract boolean isGreaterThanOrEqualTo(LispStruct obj);

	public abstract RationalStruct rational();

	public RealStruct MOD(final RealStruct divisor) {
		final RealStruct result = truncate(divisor).getQuotient();
		// TODO: this doesn't return both values...
		if (!result.zerop()) {
			if (divisor.minusp()) {
				if (plusp()) {
					return (RealStruct) result.add(divisor);
				}
			} else {
				if (minusp()) {
					return (RealStruct) result.add(divisor);
				}
			}
		}
		return result;
	}

	public abstract RealStruct max(RealStruct real);

	public abstract RealStruct min(RealStruct real);

	public ComplexStruct cis() {
		return new ComplexStruct((RealStruct) cos(), (RealStruct) sin());
	}

	public abstract RealStruct atan(RealStruct real);

	public QuotientRemainderResult floor() {
		return truncate(IntegerStruct.ONE);
	}

	public QuotientRemainderResult floor(final RealStruct divisor) {

		final BigDecimal numberBigDecimal = bigDecimalValue();
		final BigDecimal quotient = numberBigDecimal.setScale(0, RoundingMode.FLOOR);

		final BigDecimal divisorBigDecimal = divisor.bigDecimalValue();
		final BigDecimal remainder = numberBigDecimal.remainder(divisorBigDecimal, MathContext.DECIMAL128);

		final IntegerStruct integerQuotient = new IntegerStruct(quotient.toBigInteger());
		return new QuotientRemainderResult(integerQuotient, new FloatStruct(remainder));
	}

	public QuotientRemainderResult ffloor() {
		return ftruncate(IntegerStruct.ONE);
	}

	public QuotientRemainderResult ffloor(final RealStruct divisor) {

		final BigDecimal numberBigDecimal = bigDecimalValue();
		final BigDecimal quotient = numberBigDecimal.setScale(0, RoundingMode.FLOOR);

		final BigDecimal divisorBigDecimal = divisor.bigDecimalValue();
		final BigDecimal remainder = numberBigDecimal.remainder(divisorBigDecimal, MathContext.DECIMAL128);

		final FloatStruct floatQuotient;
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

		final IntegerStruct integerQuotient = new IntegerStruct(quotient.toBigInteger());
		return new QuotientRemainderResult(integerQuotient, new FloatStruct(remainder));
	}

	public QuotientRemainderResult fceiling() {
		return ftruncate(IntegerStruct.ONE);
	}

	public QuotientRemainderResult fceiling(final RealStruct divisor) {

		final BigDecimal numberBigDecimal = bigDecimalValue();
		final BigDecimal quotient = numberBigDecimal.setScale(0, RoundingMode.CEILING);

		final BigDecimal divisorBigDecimal = divisor.bigDecimalValue();
		final BigDecimal remainder = numberBigDecimal.remainder(divisorBigDecimal, MathContext.DECIMAL128);

		final FloatStruct floatQuotient;
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

		final IntegerStruct integerQuotient = new IntegerStruct(quotient.toBigInteger());
		return new QuotientRemainderResult(integerQuotient, new FloatStruct(remainder));
	}

	public QuotientRemainderResult fround() {
		return ftruncate(IntegerStruct.ONE);
	}

	public QuotientRemainderResult fround(final RealStruct divisor) {

		final BigDecimal numberBigDecimal = bigDecimalValue();
		final BigDecimal quotient = numberBigDecimal.setScale(0, RoundingMode.HALF_EVEN);

		final BigDecimal divisorBigDecimal = divisor.bigDecimalValue();
		final BigDecimal remainder = numberBigDecimal.remainder(divisorBigDecimal, MathContext.DECIMAL128);

		final FloatStruct floatQuotient;
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
		return new QuotientRemainderResult(floatQuotient, new FloatStruct(remainder));
	}
}
