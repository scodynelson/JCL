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
import jcl.types.RatioType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.util.FastMath;

/**
 * The {@link RatioStruct} is the object representation of a Lisp 'ratio' type.
 */
public class RatioStruct extends RationalStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -2468768422160538347L;

	public static final RatioStruct ZERO = new RatioStruct(BigFraction.ZERO);

	public static final RatioStruct ONE = new RatioStruct(BigFraction.ONE);

	public static final RatioStruct MINUS_ONE = new RatioStruct(BigFraction.MINUS_ONE);

	/**
	 * The internal {@link BigFraction} containing the ratio contents.
	 */
	private final BigFraction bigFraction;

	/**
	 * Public constructor.
	 *
	 * @param numerator
	 * 		the numerator value of the RatioStruct
	 * @param denominator
	 * 		the denominator value of the RatioStruct
	 */
	public RatioStruct(final BigInteger numerator, final BigInteger denominator) {
		this(new BigFraction(numerator, denominator));
	}

	/**
	 * Public constructor.
	 *
	 * @param bigFraction
	 * 		the value of the RatioStruct
	 */
	public RatioStruct(final BigFraction bigFraction) {
		super(RatioType.INSTANCE, null, null);
		this.bigFraction = bigFraction;
	}

	/**
	 * Getter for ratio {@link #bigFraction} property.
	 *
	 * @return ratio {@link #bigFraction} property
	 */
	public BigFraction getBigFraction() {
		return bigFraction;
	}

	@Override
	public boolean eql(final LispStruct lispStruct) {
		return equals(lispStruct);
	}

	@Override
	public boolean equal(final LispStruct lispStruct) {
		return equals(lispStruct);
	}

	@Override
	public boolean equalp(final LispStruct lispStruct) {
		return isEqualTo(lispStruct);
	}

	@Override
	public RealStruct abs() {
		final BigFraction abs = bigFraction.abs();
		if (abs.equals(bigFraction)) {
			return this;
		}
		return new RatioStruct(abs);
	}

	@Override
	public double doubleValue() {
		return bigFraction.doubleValue();
	}

	@Override
	public BigDecimal bigDecimalValue() {
		return bigFraction.bigDecimalValue();
	}

	@Override
	public boolean plusp() {
		final BigFraction abs = bigFraction.abs();
		return abs.equals(bigFraction);
	}

	@Override
	public boolean minusp() {
		final BigFraction abs = bigFraction.abs();
		return !abs.equals(bigFraction);
	}

	@Override
	public RationalStruct rational() {
		return this;
	}

	@Override
	public RealStruct max(final RealStruct real) {
		if (real instanceof IntegerStruct) {
			final BigDecimal asBigDecimal = bigFraction.bigDecimalValue();
			final BigDecimal max = asBigDecimal.max(new BigDecimal(((IntegerStruct) real).getBigInteger()));
			if (Objects.equals(asBigDecimal, max)) {
				return this;
			} else {
				return real;
			}
		} else if (real instanceof FloatStruct) {
			final BigDecimal asBigDecimal = bigFraction.bigDecimalValue();
			final BigDecimal max = asBigDecimal.max(((FloatStruct) real).getBigDecimal());
			if (Objects.equals(asBigDecimal, max)) {
				return this;
			} else {
				return real;
			}
		} else {
			final BigDecimal asBigDecimal = bigFraction.bigDecimalValue();
			final BigDecimal max = asBigDecimal.max(((RatioStruct) real).bigFraction.bigDecimalValue());
			if (Objects.equals(asBigDecimal, max)) {
				return this;
			} else {
				return real;
			}
		}
	}

	@Override
	public RealStruct min(final RealStruct real) {
		if (real instanceof IntegerStruct) {
			final BigDecimal asBigDecimal = bigFraction.bigDecimalValue();
			final BigDecimal min = asBigDecimal.min(new BigDecimal(((IntegerStruct) real).getBigInteger()));
			if (Objects.equals(asBigDecimal, min)) {
				return this;
			} else {
				return real;
			}
		} else if (real instanceof FloatStruct) {
			final BigDecimal asBigDecimal = bigFraction.bigDecimalValue();
			final BigDecimal min = asBigDecimal.min(((FloatStruct) real).getBigDecimal());
			if (Objects.equals(asBigDecimal, min)) {
				return this;
			} else {
				return real;
			}
		} else {
			final BigDecimal asBigDecimal = bigFraction.bigDecimalValue();
			final BigDecimal min = asBigDecimal.min(((RatioStruct) real).bigFraction.bigDecimalValue());
			if (Objects.equals(asBigDecimal, min)) {
				return this;
			} else {
				return real;
			}
		}
	}

	@Override
	public IntegerStruct numerator() {
		return new IntegerStruct(bigFraction.getNumerator());
	}

	@Override
	public IntegerStruct denominator() {
		return new IntegerStruct(bigFraction.getDenominator());
	}

	@Override
	public boolean zerop() {
		return BigFraction.ZERO.equals(bigFraction);
	}

	@Override
	public NumberStruct signum() {
		if (zerop()) {
			return this;
		} else if (plusp()) {
			return ONE;
		} else {
			return MINUS_ONE;
		}
	}

	@Override
	public NumberStruct add(final NumberStruct number) {
		if (number instanceof IntegerStruct) {
			final BigFraction n = bigFraction.add(((IntegerStruct) number).getBigInteger());
			return new RatioStruct(n);
		}
		if (number instanceof RatioStruct) {
			final BigFraction n = bigFraction.add(((RatioStruct) number).bigFraction);
			return new RatioStruct(n);
		}
		if (number instanceof FloatStruct) {
			return new FloatStruct(bigFraction.bigDecimalValue().add(((FloatStruct) number).getBigDecimal()));
		}
		if (number instanceof ComplexStruct) {
			final ComplexStruct c = (ComplexStruct) number;
			return new ComplexStruct((RealStruct) add(c.getReal()), c.getImaginary());
		}
		throw new TypeErrorException("Not of type NUMBER");
	}

	@Override
	public NumberStruct subtract(final NumberStruct number) {
		if (number instanceof IntegerStruct) {
			final BigFraction n = bigFraction.subtract(((IntegerStruct) number).getBigInteger());
			return new RatioStruct(n);
		}
		if (number instanceof RatioStruct) {
			final BigFraction n = bigFraction.subtract(((RatioStruct) number).bigFraction);
			return new RatioStruct(n);
		}
		if (number instanceof FloatStruct) {
			return new FloatStruct(bigFraction.bigDecimalValue().subtract(((FloatStruct) number).getBigDecimal()));
		}
		if (number instanceof ComplexStruct) {
			final ComplexStruct c = (ComplexStruct) number;
			return new ComplexStruct((RealStruct) subtract(c.getReal()),
					(RealStruct) new IntegerStruct(BigInteger.ZERO).subtract(c.getImaginary()));
		}
		throw new TypeErrorException("Not of type NUMBER");
	}

	@Override
	public NumberStruct multiply(final NumberStruct number) {
		if (number instanceof IntegerStruct) {
			final BigFraction n = bigFraction.multiply(((IntegerStruct) number).getBigInteger());
			return new RatioStruct(n);
		}
		if (number instanceof RatioStruct) {
			final BigFraction n = bigFraction.multiply(((RatioStruct) number).bigFraction);
			return new RatioStruct(n);
		}
		if (number instanceof FloatStruct) {
			return new FloatStruct(bigFraction.bigDecimalValue().multiply(((FloatStruct) number).getBigDecimal()));
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
			final BigFraction n = bigFraction.divide(((IntegerStruct) number).getBigInteger());
			return new RatioStruct(n);
		}
		if (number instanceof RatioStruct) {
			final BigFraction n = bigFraction.divide(((RatioStruct) number).bigFraction);
			return new RatioStruct(n);
		}
		if (number instanceof FloatStruct) {
			if (number.zerop()) {
				throw new RuntimeException("division by zero");
			}
			return new FloatStruct(bigFraction.bigDecimalValue().divide(((FloatStruct) number).getBigDecimal(), MathContext.DECIMAL128));
		}
		if (number instanceof ComplexStruct) {
			final ComplexStruct c = (ComplexStruct) number;
			// numerator
			final NumberStruct realPart = multiply(c.getReal());
			final NumberStruct imagPart = new IntegerStruct(BigInteger.ZERO).subtract(this).multiply(c.getImaginary());
			// denominator
			NumberStruct d = c.getReal().multiply(c.getReal());
			d = d.add(c.getImaginary().multiply(c.getImaginary()));
			return new ComplexStruct((RealStruct) realPart.divide(d), (RealStruct) imagPart.divide(d));
		}
		throw new TypeErrorException("Not of type NUMBER");
	}

	@Override
	public boolean isEqualTo(final LispStruct obj) {
		if (obj instanceof RatioStruct) {
			return bigFraction.equals(((RatioStruct) obj).bigFraction);
		}
		if (obj instanceof FloatStruct) {
			return isEqualTo(((FloatStruct) obj).rational());
		}
		if (obj instanceof IntegerStruct) {
			return bigFraction.equals(new BigFraction(((IntegerStruct) obj).getBigInteger()));
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
			final BigFraction n = new BigFraction(((IntegerStruct) obj).getBigInteger());
			return bigFraction.compareTo(n) < 0;
		}
		if (obj instanceof RatioStruct) {
			return bigFraction.compareTo(((RatioStruct) obj).bigFraction) < 0;
		}
		if (obj instanceof FloatStruct) {
			return isLessThan(((FloatStruct) obj).rational());
		}
		throw new TypeErrorException("Not of type REAL");
	}

	@Override
	public boolean isGreaterThan(final LispStruct obj) {
		if (obj instanceof IntegerStruct) {
			final BigFraction n = new BigFraction(((IntegerStruct) obj).getBigInteger());
			return bigFraction.compareTo(n) > 0;
		}
		if (obj instanceof RatioStruct) {
			return bigFraction.compareTo(((RatioStruct) obj).bigFraction) > 0;
		}
		if (obj instanceof FloatStruct) {
			return isGreaterThan(((FloatStruct) obj).rational());
		}
		throw new TypeErrorException("Not of type REAL");
	}

	@Override
	public boolean isLessThanOrEqualTo(final LispStruct obj) {
		if (obj instanceof IntegerStruct) {
			final BigFraction n = new BigFraction(((IntegerStruct) obj).getBigInteger());
			return bigFraction.compareTo(n) <= 0;
		}
		if (obj instanceof RatioStruct) {
			return bigFraction.compareTo(((RatioStruct) obj).bigFraction) <= 0;
		}
		if (obj instanceof FloatStruct) {
			return isLessThanOrEqualTo(((FloatStruct) obj).rational());
		}
		throw new TypeErrorException("Not of type REAL");
	}

	@Override
	public boolean isGreaterThanOrEqualTo(final LispStruct obj) {
		if (obj instanceof IntegerStruct) {
			final BigFraction n = new BigFraction(((IntegerStruct) obj).getBigInteger());
			return bigFraction.compareTo(n) >= 0;
		}
		if (obj instanceof RatioStruct) {
			return bigFraction.compareTo(((RatioStruct) obj).bigFraction) >= 0;
		}
		if (obj instanceof FloatStruct) {
			return isGreaterThanOrEqualTo(((FloatStruct) obj).rational());
		}
		throw new TypeErrorException("Not of type REAL");
	}

	@Override
	public NumberStruct expt(final NumberStruct power) {
		if (power.zerop()) {
			if (power instanceof IntegerStruct) {
				return IntegerStruct.ONE;
			}
			return FloatStruct.ONE;
		}

		if (zerop() || isEqualTo(ONE)) {
			return this;
		}

		if (power instanceof IntegerStruct) {
			// exact math version
			return intexp(this, (IntegerStruct) power);
		} else if (power instanceof ComplexStruct) {
			final ComplexStruct powerComplex = (ComplexStruct) power;

			final FloatStruct real = new FloatStruct(powerComplex.getReal().bigDecimalValue());
			final FloatStruct imaginary = new FloatStruct(powerComplex.getImaginary().bigDecimalValue());
			final ComplexStruct newPowerComplex = new ComplexStruct(real, imaginary);

			final RealStruct base = new FloatStruct(bigDecimalValue());
			return newPowerComplex.multiply(base.log()).exp();
		} else {
			final double x = doubleValue();
			final double y = ((RealStruct) power).doubleValue();

			double r = FastMath.pow(x, y);
			if (Double.isNaN(r)) {
				if (x < 0) {
					r = FastMath.pow(-x, y);
					final double realPart = r * FastMath.cos(y * Math.PI);
					final double imagPart = r * FastMath.sin(y * Math.PI);

					final FloatStruct real = new FloatStruct(BigDecimal.valueOf(realPart));
					final FloatStruct imaginary = new FloatStruct(BigDecimal.valueOf(imagPart));
					return new ComplexStruct(real, imaginary);
				}
			}
			return new FloatStruct(BigDecimal.valueOf(r));
		}
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(bigFraction)
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
		final RatioStruct rhs = (RatioStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(bigFraction, rhs.bigFraction)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(bigFraction)
		                                                                .toString();
	}
}
