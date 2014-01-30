package jcl.types.numbers;

import jcl.types.T;
import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.CompoundTypeSpecifier;
import jcl.types.typespecifiers.designator.IntervalDesignator;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.math.BigInteger;

/**
 * An {@code Integer} is a mathematical integer. There is no limit on the magnitude of an {@code Integer}.
 * <p/>
 * The types {@code Fixnum} and {@code Bignum} form an exhaustive partition of type {@code Integer}.
 */
public interface Integer extends Rational, Real, Number, T {

	Integer INSTANCE = new Factory.IntegerImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Integer> {

		@Override
		public Integer getInstance() {
			return INSTANCE;
		}

		/**
		 * Gets instance of compound {@code Integer} type.
		 *
		 * @param lowerBound the lower bound that this {@code Integer} type includes
		 * @param upperBound the upper bound that this {@code Integer} type includes
		 * @return the newly created compound {@code Integer} type
		 */
		public static Integer getInstance(final BigInteger lowerBound, final BigInteger upperBound) {
			return new IntegerImpl(lowerBound, true, upperBound, true);
		}

		/**
		 * Gets instance of compound {@code Integer} type.
		 *
		 * @param lowerBound     the lower bound that this {@code Integer} type includes
		 * @param lowerInclusive whether to include the lower bound in the interval
		 * @param upperBound     the upper bound that this {@code Integer} type includes
		 * @param upperInclusive whether to include the upper bound in the interval
		 * @return the newly created compound {@code Integer} type
		 */
		public static Integer getInstance(final BigInteger lowerBound, final boolean lowerInclusive,
										  final BigInteger upperBound, final boolean upperInclusive) {
			return new IntegerImpl(lowerBound, lowerInclusive, upperBound, upperInclusive);
		}

		/**
		 * Inner {@code Integer} type implementation.
		 */
		private static class IntegerImpl implements Integer, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private final IntervalDesignator<BigInteger> intervalDesignator;

			/**
			 * Private constructor.
			 */
			private IntegerImpl() {
				intervalDesignator = null;
			}

			/**
			 * Private constructor for compound {@code Integer} type.
			 *
			 * @param lowerBound     the lower bound that this {@code Integer} type includes
			 * @param lowerInclusive whether to include the lower bound in the interval
			 * @param upperBound     the upper bound that this {@code Integer} type includes
			 * @param upperInclusive whether to include the upper bound in the interval
			 */
			private IntegerImpl(final BigInteger lowerBound, final boolean lowerInclusive,
								final BigInteger upperBound, final boolean upperInclusive) {

				BigInteger realLower = null;
				if (lowerBound != null) {
					realLower = lowerInclusive ? lowerBound : lowerBound.add(BigInteger.ONE);
				}

				BigInteger realUpper = null;
				if (lowerBound != null) {
					realUpper = upperInclusive ? upperBound : upperBound.subtract(BigInteger.ONE);
				}

				intervalDesignator = new IntervalDesignator<>(realLower, realUpper);
			}

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof Integer)) {
					return false;
				}

				final Integer integer = (Integer) obj;
				if (integer == INSTANCE) {
					return true;
				}

				if (integer instanceof IntegerImpl) {
					final IntegerImpl integerImpl = (IntegerImpl) integer;

					return (intervalDesignator == null) || intervalDesignator.equals(integerImpl.intervalDesignator);
				}

				return false;
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder()
						.append(intervalDesignator)
						.toHashCode();
			}

			@Override
			public String toString() {
				return "IntegerImpl{" +
						"intervalDesignator=" + intervalDesignator +
						'}';
			}
		}
	}
}
