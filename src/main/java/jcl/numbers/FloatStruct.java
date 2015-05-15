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
import jcl.types.FloatType;
import jcl.types.SingleFloatType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.util.FastMath;

/**
 * The {@link FloatStruct} is the object representation of a Lisp 'float' type.
 */
public class FloatStruct extends RealStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 4803312076840516559L;

	public static final FloatStruct ZERO = new FloatStruct(BigDecimal.ZERO);

	public static final FloatStruct MINUS_ZERO = new FloatStruct(BigDecimal.valueOf(-0.0));

	public static final FloatStruct ONE = new FloatStruct(BigDecimal.ONE);

	public static final FloatStruct MINUS_ONE = new FloatStruct(BigDecimal.valueOf(-1.0));

	/**
	 * The internal {@link BigDecimal} containing the float contents.
	 */
	private final BigDecimal bigDecimal;

	/**
	 * Public constructor.
	 *
	 * @param bigDecimal
	 * 		the value of the FloatStruct
	 */
	public FloatStruct(final BigDecimal bigDecimal) {
		this(SingleFloatType.INSTANCE, bigDecimal);
	}

	/**
	 * Public constructor.
	 *
	 * @param floatType
	 * 		a {@link FloatType} that represents the type of {@link FloatType}
	 * @param bigDecimal
	 * 		the value of the FloatStruct
	 */
	public FloatStruct(final FloatType floatType, final BigDecimal bigDecimal) {
		super(floatType, null, null);
		this.bigDecimal = bigDecimal;
	}

	/**
	 * Getter for float {@link #bigDecimal} property.
	 *
	 * @return float {@link #bigDecimal} property
	 */
	public BigDecimal getBigDecimal() {
		return bigDecimal;
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
		if (bigDecimal.signum() >= 0) {
			return this;
		}
		return new FloatStruct(bigDecimal.negate());
	}

	@Override
	public RationalStruct rational() {

		final BigFraction bigFraction = BigFractionUtil.getBigFraction(bigDecimal);
		if (bigFraction.getDenominator().equals(BigInteger.ONE)) {
			return new IntegerStruct(bigFraction.getNumerator());
		}
		return new RatioStruct(bigFraction);
	}

	@Override
	public RealStruct max(final RealStruct real) {
		if (real instanceof IntegerStruct) {
			final BigDecimal max = bigDecimal.max(new BigDecimal(((IntegerStruct) real).getBigInteger()));
			if (Objects.equals(bigDecimal, max)) {
				return this;
			} else {
				return real;
			}
		} else if (real instanceof RatioStruct) {
			final BigDecimal max = bigDecimal.max(((RatioStruct) real).getBigFraction().bigDecimalValue());
			if (Objects.equals(bigDecimal, max)) {
				return this;
			} else {
				return real;
			}
		} else {
			final BigDecimal max = bigDecimal.max(((FloatStruct) real).bigDecimal);
			if (Objects.equals(bigDecimal, max)) {
				return this;
			} else {
				return real;
			}
		}
	}

	@Override
	public RealStruct min(final RealStruct real) {
		if (real instanceof IntegerStruct) {
			final BigDecimal min = bigDecimal.min(new BigDecimal(((IntegerStruct) real).getBigInteger()));
			if (Objects.equals(bigDecimal, min)) {
				return this;
			} else {
				return real;
			}
		} else if (real instanceof RatioStruct) {
			final BigDecimal min = bigDecimal.min(((RatioStruct) real).getBigFraction().bigDecimalValue());
			if (Objects.equals(bigDecimal, min)) {
				return this;
			} else {
				return real;
			}
		} else {
			final BigDecimal min = bigDecimal.min(((FloatStruct) real).bigDecimal);
			if (Objects.equals(bigDecimal, min)) {
				return this;
			} else {
				return real;
			}
		}
	}

	@Override
	public double doubleValue() {
		return bigDecimal.doubleValue();
	}

	@Override
	public BigDecimal bigDecimalValue() {
		return bigDecimal;
	}

	@Override
	public boolean plusp() {
		return bigDecimal.signum() > 0;
	}

	@Override
	public boolean minusp() {
		return bigDecimal.signum() < 0;
	}

	@Override
	public boolean zerop() {
		return bigDecimal.signum() == 0;
	}

	@Override
	public NumberStruct signum() {
		if (zerop()) {
			return this;
		}

		final RealStruct abs = abs();
		return divide(abs);
	}

	@Override
	public NumberStruct add(final NumberStruct number) {
		if (number instanceof IntegerStruct) {
			return new FloatStruct(bigDecimal.add(new BigDecimal(((IntegerStruct) number).getBigInteger())));
		}
		if (number instanceof FloatStruct) {
			return new FloatStruct(bigDecimal.add(((FloatStruct) number).bigDecimal));
		}
		if (number instanceof RatioStruct) {
			return new FloatStruct(bigDecimal.add(((RatioStruct) number).getBigFraction().bigDecimalValue()));
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
			return new FloatStruct(bigDecimal.subtract(new BigDecimal(((IntegerStruct) number).getBigInteger())));
		}
		if (number instanceof FloatStruct) {
			return new FloatStruct(bigDecimal.subtract(((FloatStruct) number).bigDecimal));
		}
		if (number instanceof RatioStruct) {
			return new FloatStruct(bigDecimal.subtract(((RatioStruct) number).getBigFraction().bigDecimalValue()));
		}
		if (number instanceof ComplexStruct) {
			final ComplexStruct c = (ComplexStruct) number;
			return new ComplexStruct((RealStruct) subtract(c.getReal()), (RealStruct) new IntegerStruct(BigInteger.ZERO).subtract(c.getImaginary()));
		}
		throw new TypeErrorException("Not of type NUMBER");
	}

	@Override
	public NumberStruct multiply(final NumberStruct number) {
		if (number instanceof IntegerStruct) {
			return new FloatStruct(bigDecimal.multiply(new BigDecimal(((IntegerStruct) number).getBigInteger())));
		}
		if (number instanceof FloatStruct) {
			return new FloatStruct(bigDecimal.multiply(((FloatStruct) number).bigDecimal));
		}
		if (number instanceof RatioStruct) {
			return new FloatStruct(bigDecimal.multiply(((RatioStruct) number).getBigFraction().bigDecimalValue()));
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
			return new FloatStruct(bigDecimal.divide(new BigDecimal(((IntegerStruct) number).getBigInteger()), MathContext.DECIMAL128));
		}
		if (number instanceof FloatStruct) {
			return new FloatStruct(bigDecimal.divide(((FloatStruct) number).bigDecimal, MathContext.DECIMAL128));
		}
		if (number instanceof RatioStruct) {
			return new FloatStruct(bigDecimal.divide(((RatioStruct) number).getBigFraction().bigDecimalValue(), MathContext.DECIMAL128));
		}
		if (number instanceof ComplexStruct) {
			final ComplexStruct c = (ComplexStruct) number;
			final NumberStruct re = c.getReal();
			final NumberStruct im = c.getImaginary();
			final NumberStruct denom = re.multiply(re).add(im.multiply(im));
			final NumberStruct resX = multiply(re).divide(denom);
			final NumberStruct resY = multiply(new IntegerStruct(BigInteger.valueOf(-1))).multiply(im).divide(denom);
			return new ComplexStruct((RealStruct) resX, (RealStruct) resY);
		}
		throw new TypeErrorException("Not of type NUMBER");
	}

	@Override
	public boolean isEqualTo(final LispStruct obj) {
		if (obj instanceof IntegerStruct) {
			return bigDecimal.compareTo(new BigDecimal(((IntegerStruct) obj).getBigInteger())) == 0;
		}
		if (obj instanceof FloatStruct) {
			return bigDecimal.compareTo(((FloatStruct) obj).bigDecimal) == 0;
		}
		if (obj instanceof RatioStruct) {
			return rational().isEqualTo(obj);
		}
		if (obj instanceof ComplexStruct) {
			return ((ComplexStruct) obj).isEqualTo(this);
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
			return bigDecimal.compareTo(new BigDecimal(((IntegerStruct) obj).getBigInteger())) < 0;
		}
		if (obj instanceof FloatStruct) {
			return bigDecimal.compareTo(((FloatStruct) obj).bigDecimal) < 0;
		}
		if (obj instanceof RatioStruct) {
			return rational().isLessThan(obj);
		}
		throw new TypeErrorException("Not of type REAL");
	}

	@Override
	public boolean isGreaterThan(final LispStruct obj) {
		if (obj instanceof IntegerStruct) {
			return bigDecimal.compareTo(new BigDecimal(((IntegerStruct) obj).getBigInteger())) > 0;
		}
		if (obj instanceof FloatStruct) {
			return bigDecimal.compareTo(((FloatStruct) obj).bigDecimal) > 0;
		}
		if (obj instanceof RatioStruct) {
			return rational().isGreaterThan(obj);
		}
		throw new TypeErrorException("Not of type REAL");
	}

	@Override
	public boolean isLessThanOrEqualTo(final LispStruct obj) {
		if (obj instanceof IntegerStruct) {
			return bigDecimal.compareTo(new BigDecimal(((IntegerStruct) obj).getBigInteger())) <= 0;
		}
		if (obj instanceof FloatStruct) {
			return bigDecimal.compareTo(((FloatStruct) obj).bigDecimal) <= 0;
		}
		if (obj instanceof RatioStruct) {
			return rational().isLessThanOrEqualTo(obj);
		}
		throw new TypeErrorException("Not of type REAL");
	}

	@Override
	public boolean isGreaterThanOrEqualTo(final LispStruct obj) {
		if (obj instanceof IntegerStruct) {
			return bigDecimal.compareTo(new BigDecimal(((IntegerStruct) obj).getBigInteger())) >= 0;
		}
		if (obj instanceof FloatStruct) {
			return bigDecimal.compareTo(((FloatStruct) obj).bigDecimal) >= 0;
		}
		if (obj instanceof RatioStruct) {
			return rational().isGreaterThanOrEqualTo(obj);
		}
		throw new TypeErrorException("Not of type REAL");
	}

	@Override
	public NumberStruct expt(final NumberStruct power) {
		if (power.zerop()) {
			return ONE;
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
		                            .append(bigDecimal)
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
		final FloatStruct rhs = (FloatStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(bigDecimal, rhs.bigDecimal)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(bigDecimal)
		                                                                .toString();
	}
}
