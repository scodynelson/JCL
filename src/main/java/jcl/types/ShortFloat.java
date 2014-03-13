package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.CompoundTypeSpecifier;
import jcl.types.typespecifiers.designator.IntervalDesignator;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.String;
import java.math.BigDecimal;

/**
 * A {@code ShortFloat} is a {@code Float} type with a minimum of 13 precision bits and 5 exponent bits.
 * <p/>
 * {@code ShortFloat} -> {@code Float} -> {@code Real} -> {@code Number} -> {@code T}
 */
public interface ShortFloat extends Float {

	ShortFloat INSTANCE = new Factory.ShortFloatImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<ShortFloat> {

		@Override
		public ShortFloat getInstance() {
			return INSTANCE;
		}

		/**
		 * Gets instance of compound {@code ShortFloat} type.
		 *
		 * @param lowerBound the lower bound that this {@code ShortFloat} type includes
		 * @param upperBound the upper bound that this {@code ShortFloat} type includes
		 * @return the newly created compound {@code ShortFloat} type
		 */
		public static ShortFloat getInstance(final BigDecimal lowerBound, final BigDecimal upperBound) {
			return ShortFloatImpl.getInstance(lowerBound, true, upperBound, true);
		}

		/**
		 * Gets instance of compound {@code ShortFloat} type.
		 *
		 * @param lowerBound     the lower bound that this {@code ShortFloat} type includes
		 * @param lowerInclusive whether to include the lower bound in the interval
		 * @param upperBound     the upper bound that this {@code ShortFloat} type includes
		 * @param upperInclusive whether to include the upper bound in the interval
		 * @return the newly created compound {@code ShortFloat} type
		 */
		public static ShortFloat getInstance(final BigDecimal lowerBound, final boolean lowerInclusive,
											 final BigDecimal upperBound, final boolean upperInclusive) {
			return ShortFloatImpl.getInstance(lowerBound, lowerInclusive, upperBound, upperInclusive);
		}

		/**
		 * Inner {@code ShortFloat} type implementation.
		 */
		private static class ShortFloatImpl implements ShortFloat, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private final IntervalDesignator<BigDecimal> intervalDesignator;

			/**
			 * Private constructor.
			 */
			private ShortFloatImpl() {
				intervalDesignator = null;
			}

			/**
			 * Private constructor for compound {@code ShortFloat} type.
			 *
			 * @param lowerBound     the lower bound that this {@code ShortFloat} type includes
			 * @param lowerInclusive whether to include the lower bound in the interval
			 * @param upperBound     the upper bound that this {@code ShortFloat} type includes
			 * @param upperInclusive whether to include the upper bound in the interval
			 */
			private ShortFloatImpl(final BigDecimal lowerBound, final boolean lowerInclusive,
								   final BigDecimal upperBound, final boolean upperInclusive) {

				final BigDecimal realLower = lowerInclusive ? lowerBound : lowerBound.add(BigDecimal.ONE);
				final BigDecimal realUpper = upperInclusive ? upperBound : upperBound.subtract(BigDecimal.ONE);
				intervalDesignator = new IntervalDesignator<>(realLower, realUpper);
			}

			/**
			 * Gets instance of compound {@code ShortFloat} type.
			 *
			 * @param lowerBound     the lower bound that this {@code ShortFloat} type includes
			 * @param lowerInclusive whether to include the lower bound in the interval
			 * @param upperBound     the upper bound that this {@code ShortFloat} type includes
			 * @param upperInclusive whether to include the upper bound in the interval
			 * @return the newly created compound {@code ShortFloat} type
			 */
			public static ShortFloat getInstance(final BigDecimal lowerBound, final boolean lowerInclusive,
												 final BigDecimal upperBound, final boolean upperInclusive) {
				return new ShortFloatImpl(lowerBound, lowerInclusive, upperBound, upperInclusive);
			}

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof ShortFloat)) {
					return false;
				}

				final ShortFloat shortFloat = (ShortFloat) obj;
				if (shortFloat == INSTANCE) {
					return true;
				}

				if (shortFloat instanceof ShortFloatImpl) {
					final ShortFloatImpl shortFloatImpl = (ShortFloatImpl) shortFloat;

					return (intervalDesignator == null) || intervalDesignator.equals(shortFloatImpl.intervalDesignator);
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
				return "ShortFloatImpl{"
						+ "intervalDesignator=" + intervalDesignator
						+ '}';
			}
		}
	}
}
