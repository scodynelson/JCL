package jcl.types.numbers;

import jcl.types.T;
import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.CompoundTypeSpecifier;
import jcl.types.typespecifiers.designator.IntervalDesignator;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.math.BigDecimal;

/**
 * A {@code SingleFloat} is a {@code Float} type with a minimum of 24 precision bits and 8 exponent bits.
 */
public interface SingleFloat extends Float, Real, Number, T {

	SingleFloat INSTANCE = new Factory.SingleFloatImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<SingleFloat> {

		@Override
		public SingleFloat getInstance() {
			return INSTANCE;
		}

		/**
		 * Gets instance of compound {@code SingleFloat} type.
		 *
		 * @param lowerBound the lower bound that this {@code SingleFloat} type includes
		 * @param upperBound the upper bound that this {@code SingleFloat} type includes
		 * @return the newly created compound {@code SingleFloat} type
		 */
		public static SingleFloat getInstance(final BigDecimal lowerBound, final BigDecimal upperBound) {
			return new SingleFloatImpl(lowerBound, true, upperBound, true);
		}

		/**
		 * Gets instance of compound {@code SingleFloat} type.
		 *
		 * @param lowerBound     the lower bound that this {@code SingleFloat} type includes
		 * @param lowerInclusive whether to include the lower bound in the interval
		 * @param upperBound     the upper bound that this {@code SingleFloat} type includes
		 * @param upperInclusive whether to include the upper bound in the interval
		 * @return the newly created compound {@code SingleFloat} type
		 */
		public static SingleFloat getInstance(final BigDecimal lowerBound, final boolean lowerInclusive,
											  final BigDecimal upperBound, final boolean upperInclusive) {
			return new SingleFloatImpl(lowerBound, lowerInclusive, upperBound, upperInclusive);
		}

		/**
		 * Inner {@code SingleFloat} type implementation.
		 */
		private static class SingleFloatImpl implements SingleFloat, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private final IntervalDesignator<BigDecimal> intervalDesignator;

			/**
			 * Private constructor.
			 */
			private SingleFloatImpl() {
				intervalDesignator = null;
			}

			/**
			 * Private constructor for compound {@code SingleFloat} type.
			 *
			 * @param lowerBound     the lower bound that this {@code SingleFloat} type includes
			 * @param lowerInclusive whether to include the lower bound in the interval
			 * @param upperBound     the upper bound that this {@code SingleFloat} type includes
			 * @param upperInclusive whether to include the upper bound in the interval
			 */
			private SingleFloatImpl(final BigDecimal lowerBound, final boolean lowerInclusive,
									final BigDecimal upperBound, final boolean upperInclusive) {

				final BigDecimal realLower = lowerInclusive ? lowerBound : lowerBound.add(BigDecimal.ONE);
				final BigDecimal realUpper = upperInclusive ? upperBound : upperBound.subtract(BigDecimal.ONE);
				intervalDesignator = new IntervalDesignator<>(realLower, realUpper);
			}

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof SingleFloat)) {
					return false;
				}

				final SingleFloat singleFloat = (SingleFloat) obj;
				if (singleFloat == INSTANCE) {
					return true;
				}

				if (singleFloat instanceof SingleFloatImpl) {
					final SingleFloatImpl singleFloatImpl = (SingleFloatImpl) singleFloat;

					return (intervalDesignator == null) || intervalDesignator.equals(singleFloatImpl.intervalDesignator);
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
				return "SingleFloatImpl{" +
						"intervalDesignator=" + intervalDesignator +
						'}';
			}
		}
	}
}
