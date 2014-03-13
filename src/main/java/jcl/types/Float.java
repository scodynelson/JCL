package jcl.types;

import jcl.typespecifiers.AtomicTypeSpecifier;
import jcl.typespecifiers.CompoundTypeSpecifier;
import jcl.typespecifiers.designator.IntervalDesignator;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.String;
import java.math.BigDecimal;

/**
 * A {@code Float} is a mathematical rational (but not a Common Lisp {@code Rational}) of the form s*f*b^e-p, where s is
 * +1 or -1, the sign; b is an {@code Integer} greater than 1, the base or radix of the representation; p is a positive
 * {@code Integer}, the precision (in base-b digits) of the {@code Float}; f is a positive {@code Integer} between b^p-1
 * and b^p-1 (inclusive), the significand; and e is an {@code Integer}, the exponent.
 * <p/>
 * The types {@code ShortFloat}, {@code SingleFloat}, {@code DoubleFloat}, and {@code LongFloat} are subtypes of type
 * {@code Float}.
 * <p/>
 * {@code Float} -> {@code Real} -> {@code Number} -> {@code T}
 */
public interface Float extends Real {

	Float INSTANCE = new Factory.FloatImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Float> {

		@Override
		public Float getInstance() {
			return INSTANCE;
		}

		/**
		 * Gets instance of compound {@code Float} type.
		 *
		 * @param lowerBound the lower bound that this {@code Float} type includes
		 * @param upperBound the upper bound that this {@code Float} type includes
		 * @return the newly created compound {@code Float} type
		 */
		public static Float getInstance(final BigDecimal lowerBound, final BigDecimal upperBound) {
			return FloatImpl.getInstance(lowerBound, true, upperBound, true);
		}

		/**
		 * Gets instance of compound {@code Float} type.
		 *
		 * @param lowerBound     the lower bound that this {@code Float} type includes
		 * @param lowerInclusive whether to include the lower bound in the interval
		 * @param upperBound     the upper bound that this {@code Float} type includes
		 * @param upperInclusive whether to include the upper bound in the interval
		 * @return the newly created compound {@code Float} type
		 */
		public static Float getInstance(final BigDecimal lowerBound, final boolean lowerInclusive,
										final BigDecimal upperBound, final boolean upperInclusive) {
			return FloatImpl.getInstance(lowerBound, lowerInclusive, upperBound, upperInclusive);
		}

		/**
		 * Inner {@code Float} type implementation.
		 */
		private static class FloatImpl implements Float, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private final IntervalDesignator<BigDecimal> intervalDesignator;

			/**
			 * Private constructor.
			 */
			private FloatImpl() {
				intervalDesignator = null;
			}

			/**
			 * Private constructor for compound {@code Float} type.
			 *
			 * @param lowerBound     the lower bound that this {@code Float} type includes
			 * @param lowerInclusive whether to include the lower bound in the interval
			 * @param upperBound     the upper bound that this {@code Float} type includes
			 * @param upperInclusive whether to include the upper bound in the interval
			 */
			private FloatImpl(final BigDecimal lowerBound, final boolean lowerInclusive,
							  final BigDecimal upperBound, final boolean upperInclusive) {

				final BigDecimal realLower = lowerInclusive ? lowerBound : lowerBound.add(BigDecimal.ONE);
				final BigDecimal realUpper = upperInclusive ? upperBound : upperBound.subtract(BigDecimal.ONE);
				intervalDesignator = new IntervalDesignator<>(realLower, realUpper);
			}

			/**
			 * Gets instance of compound {@code Float} type.
			 *
			 * @param lowerBound     the lower bound that this {@code Float} type includes
			 * @param lowerInclusive whether to include the lower bound in the interval
			 * @param upperBound     the upper bound that this {@code Float} type includes
			 * @param upperInclusive whether to include the upper bound in the interval
			 * @return the newly created compound {@code Float} type
			 */
			public static Float getInstance(final BigDecimal lowerBound, final boolean lowerInclusive,
											final BigDecimal upperBound, final boolean upperInclusive) {
				return new FloatImpl(lowerBound, lowerInclusive, upperBound, upperInclusive);
			}

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof Float)) {
					return false;
				}

				final Float aFloat = (Float) obj;
				if (aFloat == INSTANCE) {
					return true;
				}

				if (aFloat instanceof FloatImpl) {
					final FloatImpl aFloatImpl = (FloatImpl) aFloat;

					return (intervalDesignator == null) || intervalDesignator.equals(aFloatImpl.intervalDesignator);
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
				return "FloatImpl{"
						+ "intervalDesignator=" + intervalDesignator
						+ '}';
			}
		}
	}
}
