package jcl.types;

import jcl.structs.packages.GlobalPackageStruct;
import jcl.typespecifiers.AtomicTypeSpecifier;
import jcl.typespecifiers.CompoundTypeSpecifier;
import jcl.typespecifiers.designator.IntervalDesignator;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;
import java.math.BigDecimal;

/**
 * A {@link Float} is a mathematical rational (but not a Common Lisp {@link Rational}) of the form s*f*b^e-p, where s
 * is +1 or -1, the sign; b is an {@link Integer} greater than 1, the base or radix of the representation; p is a
 * positive {@link Integer}, the precision (in base-b digits) of the {@link Float}; f is a positive {@link Integer}
 * between b^p-1 and b^p-1 (inclusive), the significand; and e is an {@link Integer}, the exponent.
 * <p>
 * The types {@link ShortFloat}, {@link SingleFloat}, {@link DoubleFloat}, and {@link LongFloat} are subtypes of type
 * {@link Float}.
 * <p>
 * {@link Float} -> {@link Real} -> {@link Number} -> {@link T}
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
		 * Gets instance of compound {@link Float} type.
		 *
		 * @param lowerBound
		 * 		the lower bound that this {@link Float} type includes
		 * @param upperBound
		 * 		the upper bound that this {@link Float} type includes
		 *
		 * @return the newly created compound {@link Float} type
		 */
		public static Float getInstance(final BigDecimal lowerBound, final BigDecimal upperBound) {
			return FloatImpl.getInstance(lowerBound, true, upperBound, true);
		}

		/**
		 * Gets instance of compound {@link Float} type.
		 *
		 * @param lowerBound
		 * 		the lower bound that this {@link Float} type includes
		 * @param lowerInclusive
		 * 		whether to include the lower bound in the interval
		 * @param upperBound
		 * 		the upper bound that this {@link Float} type includes
		 * @param upperInclusive
		 * 		whether to include the upper bound in the interval
		 *
		 * @return the newly created compound {@link Float} type
		 */
		public static Float getInstance(final BigDecimal lowerBound, final boolean lowerInclusive,
		                                final BigDecimal upperBound, final boolean upperInclusive) {
			return FloatImpl.getInstance(lowerBound, lowerInclusive, upperBound, upperInclusive);
		}

		/**
		 * Inner {@link Float} type implementation.
		 */
		private static class FloatImpl extends TypeBaseClass implements Float, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private final IntervalDesignator<BigDecimal> intervalDesignator;

			/**
			 * Private constructor.
			 */
			private FloatImpl() {
				super("FLOAT", GlobalPackageStruct.COMMON_LISP);
				intervalDesignator = null;
			}

			/**
			 * Private constructor for compound {@link Float} type.
			 *
			 * @param lowerBound
			 * 		the lower bound that this {@link Float} type includes
			 * @param lowerInclusive
			 * 		whether to include the lower bound in the interval
			 * @param upperBound
			 * 		the upper bound that this {@link Float} type includes
			 * @param upperInclusive
			 * 		whether to include the upper bound in the interval
			 */
			private FloatImpl(final BigDecimal lowerBound, final boolean lowerInclusive,
			                  final BigDecimal upperBound, final boolean upperInclusive) {
				super("FLOAT", GlobalPackageStruct.COMMON_LISP);

				final BigDecimal realLower = lowerInclusive ? lowerBound : lowerBound.add(BigDecimal.ONE);
				final BigDecimal realUpper = upperInclusive ? upperBound : upperBound.subtract(BigDecimal.ONE);
				intervalDesignator = new IntervalDesignator<>(realLower, realUpper);
			}

			/**
			 * Gets instance of compound {@link Float} type.
			 *
			 * @param lowerBound
			 * 		the lower bound that this {@link Float} type includes
			 * @param lowerInclusive
			 * 		whether to include the lower bound in the interval
			 * @param upperBound
			 * 		the upper bound that this {@link Float} type includes
			 * @param upperInclusive
			 * 		whether to include the upper bound in the interval
			 *
			 * @return the newly created compound {@link Float} type
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
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}
