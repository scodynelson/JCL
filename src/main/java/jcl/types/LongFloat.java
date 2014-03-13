package jcl.types;

import jcl.typespecifiers.AtomicTypeSpecifier;
import jcl.typespecifiers.CompoundTypeSpecifier;
import jcl.typespecifiers.designator.IntervalDesignator;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.String;
import java.math.BigDecimal;

/**
 * A {@code LongFloat} is a {@code Float} type with a minimum of 50 precision bits and 8 exponent bits.
 * <p/>
 * {@code LongFloat} -> {@code Float} -> {@code Real} -> {@code Number} -> {@code T}
 */
public interface LongFloat extends Float {

	LongFloat INSTANCE = new Factory.LongFloatImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<LongFloat> {

		@Override
		public LongFloat getInstance() {
			return INSTANCE;
		}

		/**
		 * Gets instance of compound {@code LongFloat} type.
		 *
		 * @param lowerBound the lower bound that this {@code LongFloat} type includes
		 * @param upperBound the upper bound that this {@code LongFloat} type includes
		 * @return the newly created compound {@code LongFloat} type
		 */
		public static LongFloat getInstance(final BigDecimal lowerBound, final BigDecimal upperBound) {
			return LongFloatImpl.getInstance(lowerBound, true, upperBound, true);
		}

		/**
		 * Gets instance of compound {@code LongFloat} type.
		 *
		 * @param lowerBound     the lower bound that this {@code LongFloat} type includes
		 * @param lowerInclusive whether to include the lower bound in the interval
		 * @param upperBound     the upper bound that this {@code LongFloat} type includes
		 * @param upperInclusive whether to include the upper bound in the interval
		 * @return the newly created compound {@code LongFloat} type
		 */
		public static LongFloat getInstance(final BigDecimal lowerBound, final boolean lowerInclusive,
											final BigDecimal upperBound, final boolean upperInclusive) {
			return LongFloatImpl.getInstance(lowerBound, lowerInclusive, upperBound, upperInclusive);
		}

		/**
		 * Inner {@code LongFloat} type implementation.
		 */
		private static class LongFloatImpl implements LongFloat, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private final IntervalDesignator<BigDecimal> intervalDesignator;

			/**
			 * Private constructor.
			 */
			private LongFloatImpl() {
				intervalDesignator = null;
			}

			/**
			 * Private constructor for compound {@code LongFloat} type.
			 *
			 * @param lowerBound     the lower bound that this {@code LongFloat} type includes
			 * @param lowerInclusive whether to include the lower bound in the interval
			 * @param upperBound     the upper bound that this {@code LongFloat} type includes
			 * @param upperInclusive whether to include the upper bound in the interval
			 */
			private LongFloatImpl(final BigDecimal lowerBound, final boolean lowerInclusive,
								  final BigDecimal upperBound, final boolean upperInclusive) {

				final BigDecimal realLower = lowerInclusive ? lowerBound : lowerBound.add(BigDecimal.ONE);
				final BigDecimal realUpper = upperInclusive ? upperBound : upperBound.subtract(BigDecimal.ONE);
				intervalDesignator = new IntervalDesignator<>(realLower, realUpper);
			}

			/**
			 * Gets instance of compound {@code LongFloat} type.
			 *
			 * @param lowerBound     the lower bound that this {@code LongFloat} type includes
			 * @param lowerInclusive whether to include the lower bound in the interval
			 * @param upperBound     the upper bound that this {@code LongFloat} type includes
			 * @param upperInclusive whether to include the upper bound in the interval
			 * @return the newly created compound {@code LongFloat} type
			 */
			public static LongFloat getInstance(final BigDecimal lowerBound, final boolean lowerInclusive,
												final BigDecimal upperBound, final boolean upperInclusive) {
				return new LongFloatImpl(lowerBound, lowerInclusive, upperBound, upperInclusive);
			}

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof LongFloat)) {
					return false;
				}

				final LongFloat longFloat = (LongFloat) obj;
				if (longFloat == INSTANCE) {
					return true;
				}

				if (longFloat instanceof LongFloatImpl) {
					final LongFloatImpl longFloatImpl = (LongFloatImpl) longFloat;

					return (intervalDesignator == null) || intervalDesignator.equals(longFloatImpl.intervalDesignator);
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
				return "LongFloatImpl{"
						+ "intervalDesignator=" + intervalDesignator
						+ '}';
			}
		}
	}
}
