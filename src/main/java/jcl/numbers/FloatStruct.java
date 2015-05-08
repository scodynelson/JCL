/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.math.RoundingMode;

import jcl.LispStruct;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.types.FloatType;
import jcl.types.SingleFloatType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.apache.commons.math3.fraction.BigFraction;

/**
 * The {@link FloatStruct} is the object representation of a Lisp 'float' type.
 */
public class FloatStruct extends RealStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 4803312076840516559L;

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
		if (bigDecimal.signum() >= 0) {
			return this;
		}
		return new FloatStruct(bigDecimal.negate());
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
	public NumberStruct add(final NumberStruct numberStruct) {
		if (numberStruct instanceof IntegerStruct) {
			return new FloatStruct(bigDecimal.add(new BigDecimal(((IntegerStruct) numberStruct).getBigInteger())));
		}
		if (numberStruct instanceof FloatStruct) {
			return new FloatStruct(bigDecimal.add(((FloatStruct) numberStruct).bigDecimal));
		}
		if (numberStruct instanceof RatioStruct) {
			return new FloatStruct(bigDecimal.add(((RatioStruct) numberStruct).getBigFraction().bigDecimalValue()));
		}
		if (numberStruct instanceof ComplexStruct) {
			final ComplexStruct c = (ComplexStruct) numberStruct;
			return new ComplexStruct((RealStruct) add(c.getReal()), c.getImaginary());
		}
		throw new TypeErrorException("Not of type NUMBER");
	}

	@Override
	public NumberStruct subtract(final NumberStruct numberStruct) {
		if (numberStruct instanceof IntegerStruct) {
			return new FloatStruct(bigDecimal.subtract(new BigDecimal(((IntegerStruct) numberStruct).getBigInteger())));
		}
		if (numberStruct instanceof FloatStruct) {
			return new FloatStruct(bigDecimal.subtract(((FloatStruct) numberStruct).bigDecimal));
		}
		if (numberStruct instanceof RatioStruct) {
			return new FloatStruct(bigDecimal.subtract(((RatioStruct) numberStruct).getBigFraction().bigDecimalValue()));
		}
		if (numberStruct instanceof ComplexStruct) {
			final ComplexStruct c = (ComplexStruct) numberStruct;
			return new ComplexStruct((RealStruct) subtract(c.getReal()), (RealStruct) new IntegerStruct(BigInteger.ZERO).subtract(c.getImaginary()));
		}
		throw new TypeErrorException("Not of type NUMBER");
	}

	@Override
	public NumberStruct multiply(final NumberStruct numberStruct) {
		if (numberStruct instanceof IntegerStruct) {
			return new FloatStruct(bigDecimal.multiply(new BigDecimal(((IntegerStruct) numberStruct).getBigInteger())));
		}
		if (numberStruct instanceof FloatStruct) {
			return new FloatStruct(bigDecimal.multiply(((FloatStruct) numberStruct).bigDecimal));
		}
		if (numberStruct instanceof RatioStruct) {
			return new FloatStruct(bigDecimal.multiply(((RatioStruct) numberStruct).getBigFraction().bigDecimalValue()));
		}
		if (numberStruct instanceof ComplexStruct) {
			final ComplexStruct c = (ComplexStruct) numberStruct;
			return new ComplexStruct((RealStruct) multiply(c.getReal()), (RealStruct) multiply(c.getImaginary()));
		}
		throw new TypeErrorException("Not of type NUMBER");
	}

	@Override
	public NumberStruct divide(final NumberStruct numberStruct) {
		if (numberStruct instanceof IntegerStruct) {
			return new FloatStruct(bigDecimal.divide(new BigDecimal(((IntegerStruct) numberStruct).getBigInteger()), MathContext.DECIMAL128));
		}
		if (numberStruct instanceof FloatStruct) {
			return new FloatStruct(bigDecimal.divide(((FloatStruct) numberStruct).bigDecimal, MathContext.DECIMAL128));
		}
		if (numberStruct instanceof RatioStruct) {
			return new FloatStruct(bigDecimal.divide(((RatioStruct) numberStruct).getBigFraction().bigDecimalValue(), MathContext.DECIMAL128));
		}
		if (numberStruct instanceof ComplexStruct) {
			final ComplexStruct c = (ComplexStruct) numberStruct;
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
	public RealStruct truncate(final RealStruct obj) {
		if (obj instanceof IntegerStruct) {
			return truncate(new FloatStruct(new BigDecimal(((IntegerStruct) obj).getBigInteger())));
		}
		if (obj instanceof RatioStruct) {
			return truncate(new FloatStruct(((RatioStruct) obj).getBigFraction().bigDecimalValue()));
		}
		if (obj instanceof FloatStruct) {

			final BigDecimal dividend = bigDecimal;
			final BigDecimal divisor = ((FloatStruct) obj).bigDecimal;

			final BigDecimal quotient;

			final BigDecimal dividendDivisorMultiply = dividend.multiply(divisor);
			if (dividendDivisorMultiply.compareTo(BigDecimal.ZERO) >= 0) {
				quotient = dividend.divide(divisor, RoundingMode.FLOOR);
			} else {
				quotient = dividend.divide(divisor, RoundingMode.CEILING);
			}

			final RealStruct value1 = new FloatStruct(quotient);
			final BigDecimal remainder = dividend.remainder(divisor, MathContext.DECIMAL128);
			final RealStruct value2 = new FloatStruct(remainder);

			return value1;
//			return value2;
		}
		throw new TypeErrorException("Not of type REAL");
	}

	public RationalStruct rational() {

		final BigFraction bigFraction = BigFractionUtil.getBigFraction(bigDecimal);
		if (bigFraction.getDenominator().equals(BigInteger.ONE)) {
			return new IntegerStruct(bigFraction.getNumerator());
		}
		return new RatioStruct(bigFraction);
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
