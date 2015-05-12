/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.util.Objects;

import jcl.LispStruct;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.types.IntegerType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.util.FastMath;

/**
 * The {@link IntegerStruct} is the object representation of a Lisp 'integer' type.
 */
public class IntegerStruct extends RationalStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -4665072618932472349L;

	/**
	 * The internal {@link BigInteger} containing the float contents.
	 */
	private final BigInteger bigInteger;

	/**
	 * Public constructor.
	 *
	 * @param bigInteger
	 * 		the value of the IntegerStruct
	 */
	public IntegerStruct(final BigInteger bigInteger) {
		this(IntegerType.INSTANCE, bigInteger);
	}

	/**
	 * Public constructor.
	 *
	 * @param integerType
	 * 		a {@link IntegerType} that represents the type of {@link IntegerType}
	 * @param bigInteger
	 * 		the value of the IntegerStruct
	 */
	public IntegerStruct(final IntegerType integerType, final BigInteger bigInteger) {
		super(integerType, null, null);
		this.bigInteger = bigInteger;
	}

	/**
	 * Getter for integer {@link #bigInteger} property.
	 *
	 * @return integer {@link #bigInteger} property
	 */
	public BigInteger getBigInteger() {
		return bigInteger;
	}

	public boolean eql(final LispStruct lispStruct) {
		return equals(lispStruct);
	}

	public boolean equal(final LispStruct lispStruct) {
		return equals(lispStruct);
	}

	public boolean equalp(final LispStruct lispStruct) {
		return isEqualTo(lispStruct);
	}

	@Override
	public RealStruct ABS() {
		if (bigInteger.signum() >= 0) {
			return this;
		}
		return new IntegerStruct(bigInteger.negate());
	}

	@Override
	public IntegerStruct numerator() {
		return this;
	}

	@Override
	public IntegerStruct denominator() {
		return new IntegerStruct(BigInteger.ONE);
	}

	public boolean evenp() {
		return !bigInteger.testBit(0);
	}

	public boolean oddp() {
		return bigInteger.testBit(0);
	}

	@Override
	public boolean plusp() {
		return bigInteger.signum() > 0;
	}

	@Override
	public boolean minusp() {
		return bigInteger.signum() < 0;
	}

	@Override
	public RationalStruct rational() {
		return this;
	}

	@Override
	public RealStruct max(final RealStruct real) {
		if (real instanceof FloatStruct) {
			final BigDecimal asBigDecimal = new BigDecimal(bigInteger);
			final BigDecimal max = asBigDecimal.max(((FloatStruct) real).getBigDecimal());
			if (Objects.equals(asBigDecimal, max)) {
				return this;
			} else {
				return real;
			}
		} else if (real instanceof RatioStruct) {
			final BigDecimal asBigDecimal = new BigDecimal(bigInteger);
			final BigDecimal max = asBigDecimal.max(((RatioStruct) real).getBigFraction().bigDecimalValue());
			if (Objects.equals(asBigDecimal, max)) {
				return this;
			} else {
				return real;
			}
		} else {
			final BigInteger max = bigInteger.max(((IntegerStruct) real).bigInteger);
			if (Objects.equals(bigInteger, max)) {
				return this;
			} else {
				return real;
			}
		}
	}

	@Override
	public RealStruct min(final RealStruct real) {
		if (real instanceof FloatStruct) {
			final BigDecimal asBigDecimal = new BigDecimal(bigInteger);
			final BigDecimal min = asBigDecimal.max(((FloatStruct) real).getBigDecimal());
			if (Objects.equals(asBigDecimal, min)) {
				return this;
			} else {
				return real;
			}
		} else if (real instanceof RatioStruct) {
			final BigDecimal asBigDecimal = new BigDecimal(bigInteger);
			final BigDecimal min = asBigDecimal.max(((RatioStruct) real).getBigFraction().bigDecimalValue());
			if (Objects.equals(asBigDecimal, min)) {
				return this;
			} else {
				return real;
			}
		} else {
			final BigInteger min = bigInteger.max(((IntegerStruct) real).bigInteger);
			if (Objects.equals(bigInteger, min)) {
				return this;
			} else {
				return real;
			}
		}
	}

	@Override
	public boolean zerop() {
		return bigInteger.signum() == 0;
	}

	@Override
	public NumberStruct add(final NumberStruct number) {
		if (number instanceof IntegerStruct) {
			return new IntegerStruct(bigInteger.add(((IntegerStruct) number).bigInteger));
		}
		if (number instanceof RatioStruct) {
			final BigFraction bigFraction = ((RatioStruct) number).getBigFraction();
			final BigInteger numerator = bigFraction.getNumerator();
			final BigInteger denominator = bigFraction.getDenominator();
			return number(bigInteger.multiply(denominator).add(numerator), denominator);
		}
		if (number instanceof FloatStruct) {
			return new FloatStruct(new BigDecimal(bigInteger).add(((FloatStruct) number).getBigDecimal()));
		}
		if (number instanceof ComplexStruct) {
			final ComplexStruct c = (ComplexStruct) number;
			return new ComplexStruct((RealStruct) add(c.getReal()), c.getImaginary());
		}

		throw new TypeErrorException("Not of type NUMBER");
	}

	public static RationalStruct number(BigInteger numerator, BigInteger denominator) {

		if (denominator.signum() == 0) {
			throw new RuntimeException("division by zero");
		}
		if (denominator.signum() < 0) {
			numerator = numerator.negate();
			denominator = denominator.negate();
		}
		final BigInteger gcd = numerator.gcd(denominator);
		if (!gcd.equals(BigInteger.ONE)) {
			numerator = numerator.divide(gcd);
			denominator = denominator.divide(gcd);
		}
		if (denominator.equals(BigInteger.ONE)) {
			return new IntegerStruct(numerator);
		} else {
			return new RatioStruct(numerator, denominator);
		}
	}

	@Override
	public NumberStruct subtract(final NumberStruct number) {
		if (number instanceof IntegerStruct) {
			return new IntegerStruct(bigInteger.subtract(((IntegerStruct) number).bigInteger));
		}
		if (number instanceof RatioStruct) {
			final BigInteger numerator = ((RatioStruct) number).getBigFraction().getNumerator();
			final BigInteger denominator = ((RatioStruct) number).getBigFraction().getDenominator();
			return number(bigInteger.multiply(denominator).subtract(numerator),
					denominator);
		}
		if (number instanceof FloatStruct) {
			return new FloatStruct(new BigDecimal(bigInteger).subtract(((FloatStruct) number).getBigDecimal()));
		}
		if (number instanceof ComplexStruct) {
			ComplexStruct c = (ComplexStruct) number;
			return new ComplexStruct((RealStruct) subtract(c.getReal()), (RealStruct) new IntegerStruct(BigInteger.ZERO).subtract(c.getImaginary()));
		}
		throw new TypeErrorException("Not of type NUMBER");
	}

	@Override
	public NumberStruct multiply(final NumberStruct number) {
		if (number instanceof IntegerStruct) {
			return new IntegerStruct(bigInteger.multiply(((IntegerStruct) number).bigInteger));
		}
		if (number instanceof RatioStruct) {
			final BigInteger n = ((RatioStruct) number).getBigFraction().getNumerator();
			return number(n.multiply(bigInteger), ((RatioStruct) number).getBigFraction().getDenominator());
		}
		if (number instanceof FloatStruct) {
			return new FloatStruct(new BigDecimal(bigInteger).multiply(((FloatStruct) number).getBigDecimal()));
		}
		if (number instanceof ComplexStruct) {
			final ComplexStruct c = (ComplexStruct) number;
			return new ComplexStruct((RealStruct) multiply(c.getReal()), (RealStruct) multiply(c.getImaginary()));
		}
		throw new TypeErrorException("Not of type NUMBER");
	}

	@Override
	public NumberStruct divide(final NumberStruct number) {
		if (number instanceof IntegerStruct) {
			return number(bigInteger, ((IntegerStruct) number).bigInteger);
		}
		if (number instanceof RatioStruct) {
			final BigInteger d = ((RatioStruct) number).getBigFraction().getDenominator();
			return number(d.multiply(bigInteger), ((RatioStruct) number).getBigFraction().getNumerator());
		}
		if (number instanceof FloatStruct) {
			return new FloatStruct(new BigDecimal(bigInteger).divide(((FloatStruct) number).getBigDecimal(), MathContext.DECIMAL128));
		}
		if (number instanceof ComplexStruct) {
			final ComplexStruct c = (ComplexStruct) number;
			final NumberStruct realPart = c.getReal();
			final NumberStruct imagPart = c.getImaginary();
			final NumberStruct denominator = realPart.multiply(realPart).add(imagPart.multiply(imagPart));
			return new ComplexStruct((RealStruct) multiply(realPart).divide(denominator), (RealStruct) new IntegerStruct(BigInteger.ZERO).subtract(multiply(imagPart).divide(denominator)));
		}
		throw new TypeErrorException("Not of type NUMBER");
	}

	@Override
	public boolean isEqualTo(final LispStruct obj) {
		if (obj instanceof IntegerStruct) {
			return bigInteger.equals(((IntegerStruct) obj).bigInteger);
		}
		if (obj instanceof FloatStruct) {
			return isEqualTo(((FloatStruct) obj).rational());
		}
		if (obj instanceof NumberStruct) {
			return false;
		}
		throw new TypeErrorException("Not of type NUMBER");
	}

	@Override
	public boolean isNotEqualTo(final LispStruct obj) {
		return !isEqualTo(obj);
	}

	@Override
	public boolean isLessThan(final LispStruct obj) {
		if (obj instanceof IntegerStruct) {
			return bigInteger.compareTo(((IntegerStruct) obj).bigInteger) < 0;
		}
		if (obj instanceof RatioStruct) {
			final BigInteger n = bigInteger.multiply(((RatioStruct) obj).getBigFraction().getDenominator());
			return n.compareTo(((RatioStruct) obj).getBigFraction().getNumerator()) < 0;
		}
		if (obj instanceof FloatStruct) {
			return isLessThan(((FloatStruct) obj).rational());
		}
		throw new TypeErrorException("Not of type REAL");
	}

	@Override
	public boolean isGreaterThan(final LispStruct obj) {
		if (obj instanceof IntegerStruct) {
			return bigInteger.compareTo(((IntegerStruct) obj).bigInteger) > 0;
		}
		if (obj instanceof RatioStruct) {
			final BigInteger n = bigInteger.multiply(((RatioStruct) obj).getBigFraction().getDenominator());
			return n.compareTo(((RatioStruct) obj).getBigFraction().getNumerator()) > 0;
		}
		if (obj instanceof FloatStruct) {
			return isGreaterThan(((FloatStruct) obj).rational());
		}
		throw new TypeErrorException("Not of type REAL");
	}

	@Override
	public boolean isLessThanOrEqualTo(final LispStruct obj) {
		if (obj instanceof IntegerStruct) {
			return bigInteger.compareTo(((IntegerStruct) obj).bigInteger) <= 0;
		}
		if (obj instanceof RatioStruct) {
			final BigInteger n = bigInteger.multiply(((RatioStruct) obj).getBigFraction().getDenominator());
			return n.compareTo(((RatioStruct) obj).getBigFraction().getNumerator()) <= 0;
		}
		if (obj instanceof FloatStruct) {
			return isLessThanOrEqualTo(((FloatStruct) obj).rational());
		}
		throw new TypeErrorException("Not of type REAL");
	}

	@Override
	public boolean isGreaterThanOrEqualTo(final LispStruct obj) {
		if (obj instanceof IntegerStruct) {
			return bigInteger.compareTo(((IntegerStruct) obj).bigInteger) >= 0;
		}
		if (obj instanceof RatioStruct) {
			final BigInteger n = bigInteger.multiply(((RatioStruct) obj).getBigFraction().getDenominator());
			return n.compareTo(((RatioStruct) obj).getBigFraction().getNumerator()) >= 0;
		}
		if (obj instanceof FloatStruct) {
			return isGreaterThanOrEqualTo(((FloatStruct) obj).rational());
		}
		throw new TypeErrorException("Not of type REAL");
	}

	@Override
	public NumberStruct exp() {
		final double doubleValue = bigInteger.doubleValue();
		final double exp = FastMath.exp(doubleValue);
		return new FloatStruct(new BigDecimal(exp));
	}

	@Override
	public NumberStruct sqrt() {
		final double doubleValue = bigInteger.doubleValue();
		final double sqrt = FastMath.sqrt(doubleValue);
		return new FloatStruct(new BigDecimal(sqrt));
	}

	@Override
	public NumberStruct log() {
		final double doubleValue = bigInteger.doubleValue();
		final double log = FastMath.log(doubleValue);
		return new FloatStruct(new BigDecimal(log));
	}

	@Override
	public NumberStruct sin() {
		final double doubleValue = bigInteger.doubleValue();
		final double sin = FastMath.sin(doubleValue);
		return new FloatStruct(new BigDecimal(sin));
	}

	@Override
	public NumberStruct cos() {
		final double doubleValue = bigInteger.doubleValue();
		final double cos = FastMath.cos(doubleValue);
		return new FloatStruct(new BigDecimal(cos));
	}

	@Override
	public NumberStruct tan() {
		final double doubleValue = bigInteger.doubleValue();
		final double tan = FastMath.tan(doubleValue);
		return new FloatStruct(new BigDecimal(tan));
	}

	@Override
	public NumberStruct asin() {
		final double doubleValue = bigInteger.doubleValue();
		final double asin = FastMath.asin(doubleValue);
		return new FloatStruct(new BigDecimal(asin));
	}

	@Override
	public NumberStruct acos() {
		final double doubleValue = bigInteger.doubleValue();
		final double acos = FastMath.acos(doubleValue);
		return new FloatStruct(new BigDecimal(acos));
	}

	@Override
	public NumberStruct atan() {
		final double doubleValue = bigInteger.doubleValue();
		final double atan = FastMath.atan(doubleValue);
		return new FloatStruct(new BigDecimal(atan));
	}

	@Override
	public RealStruct atan(final RealStruct real) {
		final double doubleValue = bigInteger.doubleValue();

		final double doubleValue2;
		if (real instanceof FloatStruct) {
			doubleValue2 = ((FloatStruct) real).getBigDecimal().doubleValue();
		} else if (real instanceof RatioStruct) {
			doubleValue2 = ((RatioStruct) real).getBigFraction().doubleValue();
		} else {
			doubleValue2 = ((IntegerStruct) real).bigInteger.doubleValue();
		}

		final double atan = FastMath.atan2(doubleValue, doubleValue2);
		return new FloatStruct(new BigDecimal(atan));
	}

	@Override
	public NumberStruct sinh() {
		final double doubleValue = bigInteger.doubleValue();
		final double sinh = FastMath.sinh(doubleValue);
		return new FloatStruct(new BigDecimal(sinh));
	}

	@Override
	public NumberStruct cosh() {
		final double doubleValue = bigInteger.doubleValue();
		final double cosh = FastMath.cosh(doubleValue);
		return new FloatStruct(new BigDecimal(cosh));
	}

	@Override
	public NumberStruct tanh() {
		final double doubleValue = bigInteger.doubleValue();
		final double tanh = FastMath.tanh(doubleValue);
		return new FloatStruct(new BigDecimal(tanh));
	}

	@Override
	public NumberStruct asinh() {
		final double doubleValue = bigInteger.doubleValue();
		final double asinh = FastMath.asinh(doubleValue);
		return new FloatStruct(new BigDecimal(asinh));
	}

	@Override
	public NumberStruct acosh() {
		final double doubleValue = bigInteger.doubleValue();
		final double acosh = FastMath.acosh(doubleValue);
		return new FloatStruct(new BigDecimal(acosh));
	}

	@Override
	public NumberStruct atanh() {
		final double doubleValue = bigInteger.doubleValue();
		final double atanh = FastMath.atanh(doubleValue);
		return new FloatStruct(new BigDecimal(atanh));
	}

	@Override
	public RealStruct truncate(final RealStruct obj) {
		final RealStruct value1;
		final RealStruct value2;
		try {
			if (obj instanceof IntegerStruct) {
				final BigInteger divisor = ((IntegerStruct) obj).bigInteger;
				final BigInteger[] results = bigInteger.divideAndRemainder(divisor);
				final BigInteger quotient = results[0];
				final BigInteger remainder = results[1];
				value1 = new IntegerStruct(quotient);
				value2 = (remainder.signum() == 0) ? new IntegerStruct(BigInteger.ZERO) : new IntegerStruct(remainder);
			} else if (obj instanceof RatioStruct) {
				final RatioStruct divisor = (RatioStruct) obj;
				final RealStruct quotient = ((RealStruct) multiply(new IntegerStruct(divisor.getBigFraction().getDenominator()))).truncate(new IntegerStruct(divisor.getBigFraction().getNumerator()));
				final RealStruct remainder = (RealStruct) subtract(quotient.multiply(divisor));
				value1 = quotient;
				value2 = remainder;
			} else if (obj instanceof FloatStruct) {
				return new FloatStruct(new BigDecimal(bigInteger)).truncate(obj);
			} else {
				throw new TypeErrorException("Not of type REAL");
			}
		} catch (final ArithmeticException e) {
			if (obj.zerop()) {
				throw new RuntimeException("division by zero");
			}
			throw new RuntimeException("arithmetic error", e);
		}
		return value1;
//		return value2;
	}

	public IntegerStruct ash(final IntegerStruct obj) {
		final BigInteger n = bigInteger;
		final BigInteger count = obj.bigInteger;
		if (count.signum() > 0) {
			throw new RuntimeException("Can't represent result of left shift.");
		}
		if (count.signum() < 0) {
			return (n.signum() >= 0) ? new IntegerStruct(BigInteger.ZERO) : new IntegerStruct(BigInteger.valueOf(-1));
		} else {
			return this;
		}
	}

	public IntegerStruct LOGNOT() {
		return new IntegerStruct(bigInteger.not());
	}

	public IntegerStruct LOGAND(final IntegerStruct obj) {
		final BigInteger n = obj.bigInteger;
		return new IntegerStruct(bigInteger.and(n));
	}

	public IntegerStruct LOGIOR(final IntegerStruct obj) {
		final BigInteger n = obj.bigInteger;
		return new IntegerStruct(bigInteger.or(n));
	}

	public IntegerStruct LOGXOR(final IntegerStruct obj) {
		final BigInteger n = obj.bigInteger;
		return new IntegerStruct(bigInteger.xor(n));
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(bigInteger)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final IntegerStruct rhs = (IntegerStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(bigInteger, rhs.bigInteger)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(bigInteger)
		                                                                .toString();
	}
}
