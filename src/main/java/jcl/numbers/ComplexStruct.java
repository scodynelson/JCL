/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.numbers;

import java.math.BigDecimal;
import java.math.BigInteger;

import jcl.LispStruct;
import jcl.conditions.exceptions.TypeErrorException;
import jcl.types.ComplexType;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;
import org.apache.commons.math3.fraction.BigFraction;
import org.apache.commons.math3.util.FastMath;

/**
 * The {@link ComplexStruct} is the object representation of a Lisp 'complex' type.
 */
public class ComplexStruct extends NumberStruct {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = 7848008215064899579L;

	public static final ComplexStruct ZERO = new ComplexStruct(IntegerStruct.ZERO, IntegerStruct.ZERO);

	public static final ComplexStruct ONE = new ComplexStruct(IntegerStruct.ONE, IntegerStruct.ZERO);

	/**
	 * The {@link RealStruct} that comprises the real value of the complex.
	 */
	private final RealStruct real;

	/**
	 * The {@link RealStruct} that comprises the imaginary value of the complex.
	 */
	private final RealStruct imaginary;

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link RealStruct} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link RealStruct} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final RealStruct real, final RealStruct imaginary) {
		super(ComplexType.INSTANCE, null, null);
		this.real = real;
		this.imaginary = imaginary;
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link BigInteger} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link BigInteger} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final IntegerStruct real, final IntegerStruct imaginary) {
		super(ComplexType.INSTANCE, null, null);
		this.real = real;
		this.imaginary = imaginary;
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link BigInteger} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link BigDecimal} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final IntegerStruct real, final FloatStruct imaginary) {
		super(ComplexType.INSTANCE, null, null);
		this.imaginary = imaginary;

		final BigInteger realWithScale = real.getBigInteger().multiply(BigInteger.TEN);
		this.real = new FloatStruct(new BigDecimal(realWithScale, 1));
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link BigInteger} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link BigFraction} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final IntegerStruct real, final RatioStruct imaginary) {
		super(ComplexType.INSTANCE, null, null);
		this.real = real;
		this.imaginary = imaginary;
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link BigDecimal} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link BigInteger} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final FloatStruct real, final IntegerStruct imaginary) {
		super(ComplexType.INSTANCE, null, null);
		this.real = real;

		final BigInteger imaginaryWithScale = imaginary.getBigInteger().multiply(BigInteger.TEN);
		this.imaginary = new FloatStruct(new BigDecimal(imaginaryWithScale, 1));
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link BigDecimal} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link BigDecimal} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final FloatStruct real, final FloatStruct imaginary) {
		super(ComplexType.INSTANCE, null, null);
		this.real = real;
		this.imaginary = imaginary;
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link BigDecimal} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link BigFraction} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final FloatStruct real, final RatioStruct imaginary) {
		super(ComplexType.INSTANCE, null, null);
		this.real = real;
		this.imaginary = new FloatStruct(imaginary.getBigFraction().bigDecimalValue());
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link BigFraction} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link BigInteger} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final RatioStruct real, final IntegerStruct imaginary) {
		super(ComplexType.INSTANCE, null, null);
		this.real = real;
		this.imaginary = imaginary;
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link BigFraction} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link BigDecimal} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final RatioStruct real, final FloatStruct imaginary) {
		super(ComplexType.INSTANCE, null, null);
		this.real = new FloatStruct(real.getBigFraction().bigDecimalValue());
		this.imaginary = imaginary;
	}

	/**
	 * Public constructor.
	 *
	 * @param real
	 * 		a {@link BigFraction} that represents the value of real part of the ComplexStruct
	 * @param imaginary
	 * 		a {@link BigFraction} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final RatioStruct real, final RatioStruct imaginary) {
		super(ComplexType.INSTANCE, null, null);
		this.real = real;
		this.imaginary = imaginary;
	}

	/**
	 * Getter for complex {@link #imaginary} property.
	 *
	 * @return complex {@link #imaginary} property
	 */
	public RealStruct getReal() {
		return real;
	}

	/**
	 * Getter for complex {@link #imaginary} property.
	 *
	 * @return complex {@link #imaginary} property
	 */
	public RealStruct getImaginary() {
		return imaginary;
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
		if (real.zerop()) {
			return imaginary.ABS();
		}

		final BigDecimal bdReal;
		if (real instanceof IntegerStruct) {
			bdReal = new BigDecimal(((IntegerStruct) real).getBigInteger());
		} else if (real instanceof FloatStruct) {
			bdReal = ((FloatStruct) real).getBigDecimal();
		} else if (real instanceof RatioStruct) {
			bdReal = ((RatioStruct) real).getBigFraction().bigDecimalValue();
		} else {
			throw new TypeErrorException("Not of type REAL");
		}

		final BigDecimal bdImag;
		if (imaginary instanceof IntegerStruct) {
			bdImag = new BigDecimal(((IntegerStruct) imaginary).getBigInteger());
		} else if (imaginary instanceof FloatStruct) {
			bdImag = ((FloatStruct) imaginary).getBigDecimal();
		} else if (imaginary instanceof RatioStruct) {
			bdImag = ((RatioStruct) imaginary).getBigFraction().bigDecimalValue();
		} else {
			throw new TypeErrorException("Not of type REAL");
		}

		// TODO: can we avoid possible precision loss here???
		final double dblReal = bdReal.doubleValue();
		final double dblImag = bdImag.doubleValue();
		return new FloatStruct(BigDecimal.valueOf(StrictMath.hypot(dblReal, dblImag)));
	}

	@Override
	public boolean zerop() {
		return real.zerop() && imaginary.zerop();
	}

	@Override
	public NumberStruct add(final NumberStruct number) {
		if (number instanceof ComplexStruct) {
			final ComplexStruct c = (ComplexStruct) number;
			return new ComplexStruct((RealStruct) real.add(c.real), (RealStruct) imaginary.add(c.imaginary));
		}
		return new ComplexStruct((RealStruct) real.add(number), imaginary);
	}

	@Override
	public NumberStruct subtract(final NumberStruct number) {
		if (number instanceof ComplexStruct) {
			final ComplexStruct c = (ComplexStruct) number;
			return new ComplexStruct((RealStruct) real.subtract(c.real), (RealStruct) imaginary.subtract(c.imaginary));
		}
		return new ComplexStruct((RealStruct) real.subtract(number), imaginary);
	}

	@Override
	public NumberStruct multiply(final NumberStruct number) {
		if (number instanceof ComplexStruct) {
			final NumberStruct a = real;
			final NumberStruct b = imaginary;
			final NumberStruct c = ((ComplexStruct) number).real;
			final NumberStruct d = ((ComplexStruct) number).imaginary;
			// xy = (ac - bd) + i(ad + bc)
			// real part = ac - bd
			// imag part = ad + bc
			final NumberStruct ac = a.multiply(c);
			final NumberStruct bd = b.multiply(d);
			final NumberStruct ad = a.multiply(d);
			final NumberStruct bc = b.multiply(c);
			return new ComplexStruct((RealStruct) ac.subtract(bd), (RealStruct) ad.add(bc));
		}
		return new ComplexStruct((RealStruct) real.multiply(number), (RealStruct) imaginary.multiply(number));
	}

	@Override
	public NumberStruct divide(final NumberStruct number) {
		if (number instanceof ComplexStruct) {
			final NumberStruct a = real;
			final NumberStruct b = imaginary;
			final NumberStruct c = ((ComplexStruct) number).real;
			final NumberStruct d = ((ComplexStruct) number).imaginary;
			final NumberStruct ac = a.multiply(c);
			final NumberStruct bd = b.multiply(d);
			final NumberStruct bc = b.multiply(c);
			final NumberStruct ad = a.multiply(d);
			final NumberStruct denominator = c.multiply(c).add(d.multiply(d));
			return new ComplexStruct((RealStruct) ac.add(bd).divide(denominator), (RealStruct) bc.subtract(ad).divide(denominator));
		}
		return new ComplexStruct((RealStruct) real.divide(number), (RealStruct) imaginary.divide(number));
	}

	@Override
	public boolean isEqualTo(final LispStruct obj) {
		if (obj instanceof ComplexStruct) {
			final ComplexStruct c = (ComplexStruct) obj;
			return real.isEqualTo(c.real) && imaginary.isEqualTo(c.imaginary);
		}
		if (obj instanceof NumberStruct) {
			// obj is a number, but not complex.
			if (imaginary instanceof FloatStruct) {
				if (((FloatStruct) imaginary).getBigDecimal().compareTo(BigDecimal.ZERO) == 0) {
					if (obj instanceof IntegerStruct) {
						return new BigDecimal(((IntegerStruct) obj).getBigInteger()).compareTo(((FloatStruct) real).getBigDecimal()) == 0;
					}
					if (obj instanceof FloatStruct) {
						return ((FloatStruct) obj).getBigDecimal().compareTo(((FloatStruct) real).getBigDecimal()) == 0;
					}
				}
			}
			return false;
		}
		throw new TypeErrorException("Not of type NUMBER");
	}

	@Override
	public boolean isNotEqualTo(final LispStruct obj) {
		return !isEqualTo(obj);
	}

	@Override
	public NumberStruct exp() {
		return real.exp().multiply(imaginary.cis());
	}

	@Override
	public NumberStruct expt(final NumberStruct power) {
		if (power.zerop()) {
			if (power instanceof IntegerStruct) {
				if (real instanceof FloatStruct) {
					return ONE;
				}
				return IntegerStruct.ONE;
			}
			return FloatStruct.ONE;
		}
		if (zerop()) {
			return this;
		}
		if (isEqualTo(ONE)) {
			return this;
		}

		if ((power instanceof IntegerStruct) && (real instanceof RationalStruct)) {
			// exact math version
			return intexp(this, (IntegerStruct) power);
		}
		// for anything not a rational or complex rational, use
		// float approximation.
		boolean wantDoubleFloat = false;
		if ((power instanceof FloatStruct)
				|| ((real instanceof FloatStruct) || (imaginary instanceof FloatStruct))
				|| ((power instanceof ComplexStruct)
				&& ((((ComplexStruct) power).real instanceof FloatStruct)
				|| (((ComplexStruct) power).imaginary instanceof FloatStruct)))) {
			wantDoubleFloat = true;
		}

		final NumberStruct base;
		NumberStruct newPower = power;
		if (wantDoubleFloat) {
			if (newPower instanceof ComplexStruct) {
				final ComplexStruct powerComplex = (ComplexStruct) newPower;
				newPower = new ComplexStruct(
						new FloatStruct(BigDecimal.valueOf(powerComplex.real.doubleValue())),
						new FloatStruct(BigDecimal.valueOf(powerComplex.imaginary.doubleValue())));
			} else {
				final RealStruct powerReal = (RealStruct) newPower;
				newPower = new FloatStruct(BigDecimal.valueOf(powerReal.doubleValue()));
			}

			base = this;
		} else {
			base = this;
		}

		return newPower.multiply(base.log()).exp();
	}

	@Override
	public NumberStruct sqrt() {
		final RealStruct imagpart = imaginary;
		if (imagpart.zerop()) {
			final RealStruct realpart = real;
			if (realpart.minusp()) {
				return new ComplexStruct(imagpart, (RealStruct) IntegerStruct.ZERO.subtract(realpart).sqrt());
			} else {
				return new ComplexStruct((RealStruct) realpart.sqrt(), imagpart);
			}
		}
		return log().divide(IntegerStruct.TWO).exp();
	}

	@Override
	public NumberStruct log() {
		// TODO: check complex results when item is not complex!!!
		// TODO: no casting!!!
		final FloatStruct re = (FloatStruct) real;
		final FloatStruct im = (FloatStruct) imaginary;
		final FloatStruct phase = new FloatStruct(BigDecimal.valueOf(FastMath.atan2(im.getBigDecimal().doubleValue(), re.getBigDecimal().doubleValue())));  // atan(y/x)
		final FloatStruct abs = (FloatStruct) ABS();
		return new ComplexStruct(new FloatStruct(BigDecimal.valueOf(FastMath.log(abs.getBigDecimal().doubleValue()))), phase);
	}

	@Override
	public NumberStruct sin() {
		final NumberStruct n = multiply(new ComplexStruct(IntegerStruct.ZERO, IntegerStruct.ONE));
		NumberStruct result = n.exp();
		result = result.subtract(n.multiply(IntegerStruct.MINUS_ONE).exp());
		return result.divide(IntegerStruct.TWO.multiply(new ComplexStruct(IntegerStruct.ZERO, IntegerStruct.ONE)));
	}

	@Override
	public NumberStruct cos() {
		final NumberStruct n = multiply(new ComplexStruct(IntegerStruct.ZERO, IntegerStruct.ONE));
		NumberStruct result = n.exp();
		result = result.add(n.multiply(IntegerStruct.MINUS_ONE).exp());
		return result.divide(IntegerStruct.TWO);
	}

	@Override
	public NumberStruct tan() {
		return sin().divide(cos());
	}

	@Override
	public NumberStruct asin() {
		NumberStruct result = multiply(this);
		result = IntegerStruct.ONE.subtract(result);
		result = result.sqrt();
		NumberStruct n = new ComplexStruct(IntegerStruct.ZERO, IntegerStruct.ONE);
		n = n.multiply(this);
		result = n.add(result);
		result = result.log();
		result = result.multiply(new ComplexStruct(IntegerStruct.ZERO, IntegerStruct.MINUS_ONE));

		return result;
	}

	@Override
	public NumberStruct acos() {
		NumberStruct result = new FloatStruct(new BigDecimal(Math.PI / 2));
		result = result.subtract(asin());

		return result;
	}

	@Override
	public NumberStruct atan() {
		final RealStruct im = imaginary;
		if (im.zerop()) {
			return new ComplexStruct((RealStruct) real.atan(), im);
		}
		NumberStruct result = multiply(this);
		result = result.add(IntegerStruct.ONE);
		result = IntegerStruct.ONE.divide(result);
		result = result.sqrt();
		NumberStruct n = new ComplexStruct(IntegerStruct.ZERO, IntegerStruct.ONE);
		n = n.multiply(this);
		n = n.add(IntegerStruct.ONE);
		result = n.multiply(result);
		result = result.log();
		result = result.multiply(new ComplexStruct(IntegerStruct.ZERO, IntegerStruct.MINUS_ONE));
		return result;
	}

	@Override
	public NumberStruct sinh() {
		final RealStruct im = imaginary;
		if (im.zerop()) {
			return new ComplexStruct((RealStruct) real.sinh(), im);
		}
		NumberStruct result = exp();
		result = result.subtract(multiply(IntegerStruct.MINUS_ONE).exp());
		result = result.divide(IntegerStruct.TWO);

		return result;
	}

	@Override
	public NumberStruct cosh() {
		final RealStruct im = imaginary;
		if (im.zerop()) {
			return new ComplexStruct((RealStruct) real.cosh(), im);
		}
		NumberStruct result = exp();
		result = result.add(multiply(IntegerStruct.MINUS_ONE).exp());
		result = result.divide(IntegerStruct.TWO);

		return result;
	}

	@Override
	public NumberStruct tanh() {
		return sinh().divide(cosh());
	}

	@Override
	public NumberStruct asinh() {
		final RealStruct im = imaginary;
		if (im.zerop()) {
			return new ComplexStruct((RealStruct) real.asinh(), im);
		}
		NumberStruct result = multiply(this);
		result = IntegerStruct.ONE.add(result);
		result = result.sqrt();
		result = result.add(this);
		result = result.log();

		return result;
	}

	@Override
	public NumberStruct acosh() {
		final RealStruct im = imaginary;
		if (im.zerop()) {
			return new ComplexStruct((RealStruct) real.acosh(), im);
		}
		NumberStruct n1 = add(IntegerStruct.ONE);
		n1 = n1.divide(IntegerStruct.TWO);
		n1 = n1.sqrt();
		NumberStruct n2 = subtract(IntegerStruct.ONE);
		n2 = n2.divide(IntegerStruct.TWO);
		n2 = n2.sqrt();
		NumberStruct result = n1.add(n2);
		result = result.log();
		result = result.multiply(IntegerStruct.TWO);

		return result;
	}

	@Override
	public NumberStruct atanh() {
		final RealStruct im = imaginary;
		if (im.zerop()) {
			return new ComplexStruct((RealStruct) real.atanh(), im);
		}

		final NumberStruct n1 = IntegerStruct.ONE.add(this).log();
		final NumberStruct n2 = IntegerStruct.ONE.subtract(this).log();
		NumberStruct result = n1.subtract(n2);
		result = result.divide(IntegerStruct.TWO);

		return result;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().appendSuper(super.hashCode())
		                            .append(real)
		                            .append(imaginary)
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
		final ComplexStruct rhs = (ComplexStruct) obj;
		return new EqualsBuilder().appendSuper(super.equals(obj))
		                          .append(real, rhs.real)
		                          .append(imaginary, rhs.imaginary)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(real)
		                                                                .append(imaginary)
		                                                                .toString();
	}
}
