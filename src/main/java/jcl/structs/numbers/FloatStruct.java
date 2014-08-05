package jcl.structs.numbers;

import jcl.structs.symbols.Variable;
import jcl.types.DoubleFloat;
import jcl.types.Float;
import jcl.types.LongFloat;
import jcl.types.ShortFloat;
import jcl.types.SingleFloat;

import java.math.BigDecimal;

/**
 * The {@link FloatStruct} is the object representation of a Lisp 'float' type.
 */
public class FloatStruct extends RealStruct {

	private final BigDecimal bigDecimal;

	/**
	 * Public constructor.
	 *
	 * @param bigDecimal the value of the FloatStruct
	 */
	public FloatStruct(final BigDecimal bigDecimal) {
		this(SingleFloat.INSTANCE, bigDecimal);
	}

	/**
	 * Public constructor.
	 *
	 * @param floatFormat a {@link Float} that represents the type of {@link Float}
	 * @param bigDecimal  the value of the FloatStruct
	 */
	public FloatStruct(final Float floatFormat, final BigDecimal bigDecimal) {
		super(floatFormat, null, null);
		this.bigDecimal = bigDecimal;
	}

	/**
	 * This method returns the value of the FloatStruct.
	 *
	 * @return value of the FloatStruct
	 */
	public BigDecimal getBigDecimal() {
		return bigDecimal;
	}

	@Override
	public String printStruct() {
		final Float floatFormat = (Float) getType();
		final Float defaultFloatFormat = Variable.READ_DEFAULT_FLOAT_FORMAT.getValue();

		String bigDecimalString = bigDecimal.toString();
		if (!floatFormat.equals(defaultFloatFormat)) {
			if (floatFormat.equals(ShortFloat.INSTANCE)) {
				bigDecimalString = bigDecimalString.replace('E', 'S');
			} else if (floatFormat.equals(SingleFloat.INSTANCE)) {
				bigDecimalString = bigDecimalString.replace('E', 'F');
			} else if (floatFormat.equals(DoubleFloat.INSTANCE)) {
				bigDecimalString = bigDecimalString.replace('E', 'D');
			} else if (floatFormat.equals(LongFloat.INSTANCE)) {
				bigDecimalString = bigDecimalString.replace('E', 'L');
			}
		}

		return bigDecimalString;
	}

	@Override
	public String toString() {
		return "FloatStruct{"
				+ ", bigDecimal=" + bigDecimal
				+ '}';
	}
}
