package jcl.structs.numbers;

import org.apache.commons.math3.fraction.BigFraction;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * The {@code ComplexStruct} is the object representation of a Lisp 'complex' type.
 */
public class ComplexStruct extends NumberStruct {

	private final Number real;
	private final Number imaginary;

	/**
	 * Private constructor.
	 *
	 * @param real      a {@code Number} that represents the value of real part of the {@code ComplexStruct}
	 * @param imaginary a {@code Number} that represents the value of imaginary part {@code ComplexStruct}
	 */
	private ComplexStruct(final Number real, final Number imaginary) {
		this.real = real;
		this.imaginary = imaginary;
	}

	/**
	 * This method returns the {@code Number} value representative of the real part.
	 *
	 * @return the {@code Number} value representative of the real part
	 */
	public Number getReal() {
		return real;
	}

	/**
	 * This method returns the {@code Number} value representative of the imaginary part.
	 *
	 * @return the {@code Number} value representative of the imaginary part
	 */
	public Number getImaginary() {
		return imaginary;
	}

	@Override
	public String toString() {
		return "ComplexStruct{" +
				"real=" + real +
				", imaginary=" + imaginary +
				'}';
	}

	// BUILDERS

	/**
	 * This method gets the {@code ComplexStruct} for the provided {@code real} and {@code imaginary}.
	 *
	 * @param real      a {@code Number} that represents the value of real part of the {@code ComplexStruct}
	 * @param imaginary a {@code Number} that represents the value of imaginary part {@code ComplexStruct}
	 * @return the created {@code ComplexStruct}
	 */
	public static ComplexStruct getStruct(final BigInteger real, final BigInteger imaginary) {
		return new ComplexStruct(real, imaginary);
	}

	/**
	 * This method gets the {@code ComplexStruct} for the provided {@code real} and {@code imaginary}.
	 *
	 * @param real      a {@code Number} that represents the value of real part of the {@code ComplexStruct}
	 * @param imaginary a {@code Number} that represents the value of imaginary part {@code ComplexStruct}
	 * @return the created {@code ComplexStruct}
	 */
	public static ComplexStruct getStruct(final BigInteger real, final BigDecimal imaginary) {
		final BigInteger realWithScale = real.multiply(BigInteger.TEN);
		final BigDecimal realAsBigDecimal = new BigDecimal(realWithScale, 1);

		return new ComplexStruct(realAsBigDecimal, imaginary);
	}

	/**
	 * This method gets the {@code ComplexStruct} for the provided {@code real} and {@code imaginary}.
	 *
	 * @param real      a {@code Number} that represents the value of real part of the {@code ComplexStruct}
	 * @param imaginary a {@code Number} that represents the value of imaginary part {@code ComplexStruct}
	 * @return the created {@code ComplexStruct}
	 */
	public static ComplexStruct getStruct(final BigInteger real, final BigFraction imaginary) {
		return new ComplexStruct(real, imaginary);
	}

	/**
	 * This method gets the {@code ComplexStruct} for the provided {@code real} and {@code imaginary}.
	 *
	 * @param real      a {@code Number} that represents the value of real part of the {@code ComplexStruct}
	 * @param imaginary a {@code Number} that represents the value of imaginary part {@code ComplexStruct}
	 * @return the created {@code ComplexStruct}
	 */
	public static ComplexStruct getStruct(final BigDecimal real, final BigInteger imaginary) {
		final BigInteger imaginaryWithScale = imaginary.multiply(BigInteger.TEN);
		final BigDecimal imaginaryAsBigDecimal = new BigDecimal(imaginaryWithScale, 1);

		return new ComplexStruct(real, imaginaryAsBigDecimal);
	}

	/**
	 * This method gets the {@code ComplexStruct} for the provided {@code real} and {@code imaginary}.
	 *
	 * @param real      a {@code Number} that represents the value of real part of the {@code ComplexStruct}
	 * @param imaginary a {@code Number} that represents the value of imaginary part {@code ComplexStruct}
	 * @return the created {@code ComplexStruct}
	 */
	public static ComplexStruct getStruct(final BigDecimal real, final BigDecimal imaginary) {
		return new ComplexStruct(real, imaginary);
	}

	/**
	 * This method gets the {@code ComplexStruct} for the provided {@code real} and {@code imaginary}.
	 *
	 * @param real      a {@code Number} that represents the value of real part of the {@code ComplexStruct}
	 * @param imaginary a {@code Number} that represents the value of imaginary part {@code ComplexStruct}
	 * @return the created {@code ComplexStruct}
	 */
	public static ComplexStruct getStruct(final BigDecimal real, final BigFraction imaginary) {
		return new ComplexStruct(real, imaginary.bigDecimalValue());
	}

	/**
	 * This method gets the {@code ComplexStruct} for the provided {@code real} and {@code imaginary}.
	 *
	 * @param real      a {@code Number} that represents the value of real part of the {@code ComplexStruct}
	 * @param imaginary a {@code Number} that represents the value of imaginary part {@code ComplexStruct}
	 * @return the created {@code ComplexStruct}
	 */
	public static ComplexStruct getStruct(final BigFraction real, final BigInteger imaginary) {
		return new ComplexStruct(real, imaginary);
	}

	/**
	 * This method gets the {@code ComplexStruct} for the provided {@code real} and {@code imaginary}.
	 *
	 * @param real      a {@code Number} that represents the value of real part of the {@code ComplexStruct}
	 * @param imaginary a {@code Number} that represents the value of imaginary part {@code ComplexStruct}
	 * @return the created {@code ComplexStruct}
	 */
	public static ComplexStruct getStruct(final BigFraction real, final BigDecimal imaginary) {
		return new ComplexStruct(real.bigDecimalValue(), imaginary);
	}

	/**
	 * This method gets the {@code ComplexStruct} for the provided {@code real} and {@code imaginary}.
	 *
	 * @param real      a {@code Number} that represents the value of real part of the {@code ComplexStruct}
	 * @param imaginary a {@code Number} that represents the value of imaginary part {@code ComplexStruct}
	 * @return the created {@code ComplexStruct}
	 */
	public static ComplexStruct getStruct(final BigFraction real, final BigFraction imaginary) {
		return new ComplexStruct(real, imaginary);
	}
}
