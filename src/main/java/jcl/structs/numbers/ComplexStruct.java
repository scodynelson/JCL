package jcl.structs.numbers;

import jcl.types.Complex;
import org.apache.commons.math3.fraction.BigFraction;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * The {@link ComplexStruct} is the object representation of a Lisp 'complex' type.
 */
public class ComplexStruct extends NumberStruct {

	private final RealStruct real;
	private final RealStruct imaginary;

	/**
	 * Public constructor.
	 *
	 * @param real      a {@link BigInteger} that represents the value of real part of the ComplexStruct
	 * @param imaginary a {@link BigInteger} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final IntegerStruct real, final IntegerStruct imaginary) {
		super(Complex.INSTANCE, null, null);
		this.real = real;
		this.imaginary = imaginary;
	}

	/**
	 * Public constructor.
	 *
	 * @param real      a {@link BigInteger} that represents the value of real part of the ComplexStruct
	 * @param imaginary a {@link BigDecimal} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final IntegerStruct real, final FloatStruct imaginary) {
		super(Complex.INSTANCE, null, null);
		this.imaginary = imaginary;

		final BigInteger realWithScale = real.getBigInteger().multiply(BigInteger.TEN);
		this.real = new FloatStruct(new BigDecimal(realWithScale, 1));
	}

	/**
	 * Public constructor.
	 *
	 * @param real      a {@link BigInteger} that represents the value of real part of the ComplexStruct
	 * @param imaginary a {@link BigFraction} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final IntegerStruct real, final RatioStruct imaginary) {
		super(Complex.INSTANCE, null, null);
		this.real = real;
		this.imaginary = imaginary;
	}

	/**
	 * Public constructor.
	 *
	 * @param real      a {@link BigDecimal} that represents the value of real part of the ComplexStruct
	 * @param imaginary a {@link BigInteger} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final FloatStruct real, final IntegerStruct imaginary) {
		super(Complex.INSTANCE, null, null);
		this.real = real;

		final BigInteger imaginaryWithScale = imaginary.getBigInteger().multiply(BigInteger.TEN);
		this.imaginary = new FloatStruct(new BigDecimal(imaginaryWithScale, 1));
	}

	/**
	 * Public constructor.
	 *
	 * @param real      a {@link BigDecimal} that represents the value of real part of the ComplexStruct
	 * @param imaginary a {@link BigDecimal} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final FloatStruct real, final FloatStruct imaginary) {
		super(Complex.INSTANCE, null, null);
		this.real = real;
		this.imaginary = imaginary;
	}

	/**
	 * Public constructor.
	 *
	 * @param real      a {@link BigDecimal} that represents the value of real part of the ComplexStruct
	 * @param imaginary a {@link BigFraction} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final FloatStruct real, final RatioStruct imaginary) {
		super(Complex.INSTANCE, null, null);
		this.real = real;
		this.imaginary = new FloatStruct(imaginary.getBigFraction().bigDecimalValue());
	}

	/**
	 * Public constructor.
	 *
	 * @param real      a {@link BigFraction} that represents the value of real part of the ComplexStruct
	 * @param imaginary a {@link BigInteger} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final RatioStruct real, final IntegerStruct imaginary) {
		super(Complex.INSTANCE, null, null);
		this.real = real;
		this.imaginary = imaginary;
	}

	/**
	 * Public constructor.
	 *
	 * @param real      a {@link BigFraction} that represents the value of real part of the ComplexStruct
	 * @param imaginary a {@link BigDecimal} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final RatioStruct real, final FloatStruct imaginary) {
		super(Complex.INSTANCE, null, null);
		this.real = new FloatStruct(real.getBigFraction().bigDecimalValue());
		this.imaginary = imaginary;
	}

	/**
	 * Public constructor.
	 *
	 * @param real      a {@link BigFraction} that represents the value of real part of the ComplexStruct
	 * @param imaginary a {@link BigFraction} that represents the value of imaginary part ComplexStruct
	 */
	public ComplexStruct(final RatioStruct real, final RatioStruct imaginary) {
		super(Complex.INSTANCE, null, null);
		this.real = real;
		this.imaginary = imaginary;
	}

	/**
	 * This method returns the {@link Number} value representative of the real part.
	 *
	 * @return the {@link Number} value representative of the real part
	 */
	public RealStruct getReal() {
		return real;
	}

	/**
	 * This method returns the {@link Number} value representative of the imaginary part.
	 *
	 * @return the {@link Number} value representative of the imaginary part
	 */
	public RealStruct getImaginary() {
		return imaginary;
	}

	@Override
	public String printStruct() {
		return "#C(" + real.printStruct() + ' ' + imaginary.printStruct() + ')';
	}

	@Override
	public String toString() {
		return "ComplexStruct{"
				+ "real=" + real
				+ ", imaginary=" + imaginary
				+ '}';
	}
}
