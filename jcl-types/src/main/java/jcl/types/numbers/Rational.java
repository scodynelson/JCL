package jcl.types.numbers;

import jcl.types.T;
import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.CompoundTypeSpecifier;
import jcl.types.typespecifiers.designator.IntervalDesignator;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.math.BigInteger;

/**
 * A {@code Rational} is a {@code Number} with a canonical representation of an {@code Integer} if its value is integral, and
 * otherwise as a {@code Ratio}.
 * <p/>
 * The types {@code Integer} and {@code Ratio} are disjoint subtypes of type {@code Rational}.
 */
public interface Rational extends Real, Number, T {

	Rational INSTANCE = new Factory.RationalImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Rational> {

		@Override
		public Rational getInstance() {
			return INSTANCE;
		}

		/**
		 * Gets instance of compound {@code Rational} type.
		 *
		 * @param lowerBound the lower bound that this {@code Rational} type includes
		 * @param upperBound the upper bound that this {@code Rational} type includes
		 * @return the newly created compound {@code Rational} type
		 */
		public static Rational getInstance(final BigInteger lowerBound, final BigInteger upperBound) {
			return new RationalImpl(lowerBound, true, upperBound, true);
		}

		/**
		 * Gets instance of compound {@code Rational} type.
		 *
		 * @param lowerBound     the lower bound that this {@code Rational} type includes
		 * @param lowerInclusive whether to include the lower bound in the interval
		 * @param upperBound     the upper bound that this {@code Rational} type includes
		 * @param upperInclusive whether to include the upper bound in the interval
		 * @return the newly created compound {@code Rational} type
		 */
		public static Rational getInstance(final BigInteger lowerBound, final boolean lowerInclusive,
										   final BigInteger upperBound, final boolean upperInclusive) {
			return new RationalImpl(lowerBound, lowerInclusive, upperBound, upperInclusive);
		}

		/**
		 * Inner {@code Rational} type implementation.
		 */
		private static class RationalImpl implements Rational, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private final IntervalDesignator<BigInteger> intervalDesignator;

			/**
			 * Private constructor.
			 */
			private RationalImpl() {
				intervalDesignator = null;
			}

			/**
			 * Private constructor for compound {@code Rational} type.
			 *
			 * @param lowerBound     the lower bound that this {@code Rational} type includes
			 * @param lowerInclusive whether to include the lower bound in the interval
			 * @param upperBound     the upper bound that this {@code Rational} type includes
			 * @param upperInclusive whether to include the upper bound in the interval
			 */
			private RationalImpl(final BigInteger lowerBound, final boolean lowerInclusive,
								 final BigInteger upperBound, final boolean upperInclusive) {

				final BigInteger realLower = lowerInclusive ? lowerBound : lowerBound.add(BigInteger.ONE);
				final BigInteger realUpper = upperInclusive ? upperBound : upperBound.subtract(BigInteger.ONE);
				intervalDesignator = new IntervalDesignator<>(realLower, realUpper);
			}

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof Rational)) {
					return false;
				}

				final Rational rational = (Rational) obj;
				if (rational == INSTANCE) {
					return true;
				}

				if (rational instanceof RationalImpl) {
					final RationalImpl rationalImpl = (RationalImpl) rational;

					return (intervalDesignator == null) || intervalDesignator.equals(rationalImpl.intervalDesignator);
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
				return "RationalImpl{" +
						"intervalDesignator=" + intervalDesignator +
						'}';
			}
		}
	}
}
