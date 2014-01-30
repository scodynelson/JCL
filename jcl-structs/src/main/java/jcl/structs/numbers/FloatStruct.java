package jcl.structs.numbers;

import jcl.types.numbers.Float;
import jcl.types.numbers.SingleFloat;

import java.math.BigDecimal;

/**
 * The {@code FloatStruct} is the object representation of a Lisp 'float' type.
 */
public class FloatStruct extends RealStruct {

	private final Float floatFormat;
	private final BigDecimal bigDecimal;

	/**
	 * Private constructor.
	 *
	 * @param floatFormat a {@code Float} that represents the type of {@code Float}
	 * @param bigDecimal  the value of the {@code FloatStruct}
	 */
	private FloatStruct(final Float floatFormat, final BigDecimal bigDecimal) {
		this.floatFormat = floatFormat;
		this.bigDecimal = bigDecimal;
	}

	/**
	 * This method returns the {@code Float} that represents the type of {@code Float}.
	 *
	 * @return the {@code Float} value
	 */
	public Float getFloatFormat() {
		return floatFormat;
	}

	/**
	 * This method returns the value of the {@code FloatStruct}.
	 *
	 * @return value of the {@code FloatStruct}
	 */
	public BigDecimal getBigDecimal() {
		return bigDecimal;
	}

	@Override
	public String toString() {
		return "FloatStruct{" +
				"floatFormat=" + floatFormat +
				", bigDecimal=" + bigDecimal +
				'}';
	}

	// BUILDERS

	/**
	 * This method gets the {@code FloatStruct} for the provided {@code bigDecimal}.
	 *
	 * @param bigDecimal the value of the {@code FloatStruct}
	 * @return the created {@code FloatStruct}
	 */
	public static FloatStruct getStruct(final BigDecimal bigDecimal) {
		return new FloatStruct(SingleFloat.INSTANCE, bigDecimal);
	}

	/**
	 * This method gets the {@code FloatStruct} for the provided {@code floatFormat} and {@code bigDecimal}.
	 *
	 * @param floatFormat a {@code Float} that represents the type of {@code Float}
	 * @param bigDecimal  the value of the {@code FloatStruct}
	 * @return the created {@code FloatStruct}
	 */
	public static FloatStruct getStruct(final Float floatFormat, final BigDecimal bigDecimal) {
		return new FloatStruct(floatFormat, bigDecimal);
	}
}
