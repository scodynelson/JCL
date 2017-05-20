package jcl.lang.internal;

import java.math.BigInteger;

import jcl.lang.IntegerStruct;
import jcl.lang.classes.BuiltInClassStruct;
import jcl.type.IntegerType;

abstract class IntegerStructImpl extends BuiltInClassStruct implements IntegerStruct {

	protected IntegerStructImpl(final IntegerType type) {
		super(type, null, null);
	}

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
}
