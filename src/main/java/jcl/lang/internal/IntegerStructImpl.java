package jcl.lang.internal;

import java.math.BigInteger;

import jcl.lang.IntegerStruct;
import jcl.lang.statics.CommonLispSymbols;

abstract class IntegerStructImpl extends LispStructImpl implements IntegerStruct {

	/*
	Add Helpers
	 */

	protected static IntegerStruct addExact(final int x, final int y) {
		try {
			final int add = Math.addExact(x, y);
			return new FixnumStructImpl(add);
		} catch (final ArithmeticException ignored) {
			return addExact((long) x, y);
		}
	}

	protected static IntegerStruct addExact(final int x, final long y) {
		try {
			final long add = Math.addExact(x, y);
			return new LongnumStructImpl(add);
		} catch (final ArithmeticException ignored) {
			return addExact((long) x, y);
		}
	}

	protected static IntegerStruct addExact(final long x, final int y) {
		try {
			final long add = Math.addExact(x, y);
			return new LongnumStructImpl(add);
		} catch (final ArithmeticException ignored) {
			return addExact(x, (long) y);
		}
	}

	protected static IntegerStruct addExact(final long x, final long y) {
		try {
			final long add = Math.addExact(x, y);
			return new LongnumStructImpl(add);
		} catch (final ArithmeticException ignored) {
			final BigInteger xBI = BigInteger.valueOf(x);
			final BigInteger yBI = BigInteger.valueOf(y);
			return new BignumStructImpl(xBI.add(yBI));
		}
	}

	/*
	Subtract Helpers
	 */

	protected static IntegerStruct subtractExact(final int x, final int y) {
		try {
			final int subtract = Math.subtractExact(x, y);
			return new FixnumStructImpl(subtract);
		} catch (final ArithmeticException ignored) {
			return subtractExact((long) x, y);
		}
	}

	protected static IntegerStruct subtractExact(final int x, final long y) {
		try {
			final long subtract = Math.subtractExact(x, y);
			return new LongnumStructImpl(subtract);
		} catch (final ArithmeticException ignored) {
			return subtractExact((long) x, y);
		}
	}

	protected static IntegerStruct subtractExact(final long x, final int y) {
		try {
			final long subtract = Math.subtractExact(x, y);
			return new LongnumStructImpl(subtract);
		} catch (final ArithmeticException ignored) {
			return subtractExact(x, (long) y);
		}
	}

	protected static IntegerStruct subtractExact(final long x, final long y) {
		try {
			final long subtract = Math.subtractExact(x, y);
			return new LongnumStructImpl(subtract);
		} catch (final ArithmeticException ignored) {
			final BigInteger xBI = BigInteger.valueOf(x);
			final BigInteger yBI = BigInteger.valueOf(y);
			return new BignumStructImpl(xBI.subtract(yBI));
		}
	}

	/*
	Multiply Helpers
	 */

	protected static IntegerStruct multiplyExact(final int x, final int y) {
		try {
			final int multiply = Math.multiplyExact(x, y);
			return new FixnumStructImpl(multiply);
		} catch (final ArithmeticException ignored) {
			return multiplyExact((long) x, y);
		}
	}

	protected static IntegerStruct multiplyExact(final int x, final long y) {
		try {
			final long multiply = Math.multiplyExact(x, y);
			return new LongnumStructImpl(multiply);
		} catch (final ArithmeticException ignored) {
			return multiplyExact((long) x, y);
		}
	}

	protected static IntegerStruct multiplyExact(final long x, final int y) {
		try {
			final long multiply = Math.multiplyExact(x, y);
			return new LongnumStructImpl(multiply);
		} catch (final ArithmeticException ignored) {
			return multiplyExact(x, (long) y);
		}
	}

	protected static IntegerStruct multiplyExact(final long x, final long y) {
		try {
			final long multiply = Math.multiplyExact(x, y);
			return new LongnumStructImpl(multiply);
		} catch (final ArithmeticException ignored) {
			final BigInteger xBI = BigInteger.valueOf(x);
			final BigInteger yBI = BigInteger.valueOf(y);
			return new BignumStructImpl(xBI.multiply(yBI));
		}
	}

	/*
	LCM Helper
	 */

	protected static BigInteger lcm(final BigInteger a, final BigInteger b) {
		final BigInteger gcd = a.gcd(b);
		return a.divide(gcd).multiply(b);
	}

	/*
	INTEGER-STRUCT
	 */

	/*
		ToString
	 */

	/**
	 * Int constant for the value '2'.
	 */
	private static final int TWO_PRINTER = 2;

	/**
	 * Int constant for the value '8'.
	 */
	private static final int EIGHT_PRINTER = 8;

	/**
	 * Int constant for the value '10'.
	 */
	private static final int TEN_PRINTER = 10;

	/**
	 * Int constant for the value '16'.
	 */
	private static final int SIXTEEN_PRINTER = 16;

	@Override
	public String toString() {
		// TODO

		final boolean printRadix = CommonLispSymbols.PRINT_RADIX_VAR.getVariableValue().toJavaPBoolean();
		final int printBase = CommonLispSymbols.PRINT_BASE_VAR.getVariableValue().toJavaInt();

		final StringBuilder stringBuilder = new StringBuilder();

		if (printRadix) {
			if (printBase == TWO_PRINTER) {
				stringBuilder.append("#b");
			} else if (printBase == EIGHT_PRINTER) {
				stringBuilder.append("#o");
			} else if (printBase == SIXTEEN_PRINTER) {
				stringBuilder.append("#x");
			} else {
				stringBuilder.append('#');
				stringBuilder.append(printBase);
				stringBuilder.append('r');
			}
		}

		final BigInteger bigInteger = toJavaBigInteger();
		stringBuilder.append(bigInteger.toString(printBase));

		if (printRadix && (printBase == TEN_PRINTER)) {
			stringBuilder.append('.');
		}

		return stringBuilder.toString();
	}
}
