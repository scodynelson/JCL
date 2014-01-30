package jcl.types.numbers;

import jcl.types.T;
import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.CompoundTypeSpecifier;
import jcl.types.typespecifiers.designator.IntervalDesignator;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.math.BigDecimal;

/**
 * A {@code DoubleFloat} is a {@code Float} type with a minimum of 50 precision bits and 8 exponent bits.
 */
public interface DoubleFloat extends Float, Real, Number, T {

	DoubleFloat INSTANCE = new Factory.DoubleFloatImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<DoubleFloat> {

		@Override
		public DoubleFloat getInstance() {
			return INSTANCE;
		}

		/**
		 * Gets instance of compound {@code DoubleFloat} type.
		 *
		 * @param lowerBound the lower bound that this {@code DoubleFloat} type includes
		 * @param upperBound the upper bound that this {@code DoubleFloat} type includes
		 * @return the newly created compound {@code DoubleFloat} type
		 */
		public static DoubleFloat getInstance(final BigDecimal lowerBound, final BigDecimal upperBound) {
			return new DoubleFloatImpl(lowerBound, true, upperBound, true);
		}

		/**
		 * Gets instance of compound {@code DoubleFloat} type.
		 *
		 * @param lowerBound     the lower bound that this {@code DoubleFloat} type includes
		 * @param lowerInclusive whether to include the lower bound in the interval
		 * @param upperBound     the upper bound that this {@code DoubleFloat} type includes
		 * @param upperInclusive whether to include the upper bound in the interval
		 * @return the newly created compound {@code DoubleFloat} type
		 */
		public static DoubleFloat getInstance(final BigDecimal lowerBound, final boolean lowerInclusive,
											  final BigDecimal upperBound, final boolean upperInclusive) {
			return new DoubleFloatImpl(lowerBound, lowerInclusive, upperBound, upperInclusive);
		}

		/**
		 * Inner {@code DoubleFloat} type implementation.
		 */
		private static class DoubleFloatImpl implements DoubleFloat, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private final IntervalDesignator<BigDecimal> intervalDesignator;

			/**
			 * Private constructor.
			 */
			private DoubleFloatImpl() {
				intervalDesignator = null;
			}

			/**
			 * Private constructor for compound {@code DoubleFloat} type.
			 *
			 * @param lowerBound     the lower bound that this {@code DoubleFloat} type includes
			 * @param lowerInclusive whether to include the lower bound in the interval
			 * @param upperBound     the upper bound that this {@code DoubleFloat} type includes
			 * @param upperInclusive whether to include the upper bound in the interval
			 */
			private DoubleFloatImpl(final BigDecimal lowerBound, final boolean lowerInclusive,
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

				if (!(obj instanceof DoubleFloat)) {
					return false;
				}

				final DoubleFloat doubleFloat = (DoubleFloat) obj;
				if (doubleFloat == INSTANCE) {
					return true;
				}

				if (doubleFloat instanceof DoubleFloatImpl) {
					final DoubleFloatImpl doubleFloatImpl = (DoubleFloatImpl) doubleFloat;

					return (intervalDesignator == null) || intervalDesignator.equals(doubleFloatImpl.intervalDesignator);
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
				return "DoubleFloatImpl{" +
						"intervalDesignator=" + intervalDesignator +
						'}';
			}
		}
	}
}
