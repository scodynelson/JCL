package jcl.structs.numbers;

import jcl.structs.symbols.Variable;
import jcl.types.Integer;

import java.math.BigInteger;

/**
 * The {@link IntegerStruct} is the object representation of a Lisp 'integer' type.
 */
public class IntegerStruct extends RationalStruct {

	private final BigInteger bigInteger;

	/**
	 * Public constructor.
	 *
	 * @param bigInteger the value of the {@code IntegerStruct}
	 */
	public IntegerStruct(final BigInteger bigInteger) {
		this(Integer.INSTANCE, bigInteger);
	}

	/**
	 * Public constructor.
	 *
	 * @param integerFormat a {@link Integer} that represents the type of {@link Integer}
	 * @param bigInteger    the value of the {@code IntegerStruct}
	 */
	public IntegerStruct(final Integer integerFormat, final BigInteger bigInteger) {
		super(integerFormat, null, null);
		this.bigInteger = bigInteger;
	}

	/**
	 * This method returns the value of the {@code IntegerStruct}.
	 *
	 * @return value of the {@code IntegerStruct}
	 */
	public BigInteger getBigInteger() {
		return bigInteger;
	}

	@Override
	public String printStruct() {
		final boolean printRadix = Variable.PRINT_RADIX.getValue().booleanValue();
		final int printBase = Variable.PRINT_BASE.getValue().bigInteger.intValue();

		final StringBuilder stringBuilder = new StringBuilder();

		if (printRadix) {
			if (printBase == 2) {
				stringBuilder.append("#b");
			} else if (printBase == 8) {
				stringBuilder.append("#o");
			} else if (printBase == 16) {
				stringBuilder.append("#x");
			} else if (printBase != 10) {
				stringBuilder.append('#');
				stringBuilder.append(printBase);
				stringBuilder.append('r');
			}
		}

		stringBuilder.append(bigInteger.toString(printBase));

		if (printRadix && (printBase == 10)) {
			stringBuilder.append('.');
		}

		return stringBuilder.toString();
	}

	@Override
	public String toString() {
		return "IntegerStruct{"
				+ "bigInteger=" + bigInteger
				+ '}';
	}
}