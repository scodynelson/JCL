/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.CompoundTypeSpecifier;
import jcl.types.typespecifiers.designator.IntervalDesignator;
import org.apache.commons.lang3.builder.HashCodeBuilder;

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

	/**
	 * Singleton instance of the {@link Float} type.
	 */
	Float INSTANCE = new Factory.FloatImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Float> {

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

		@Override
		public Float getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Float} type implementation.
		 */
		private static final class FloatImpl extends TypeBaseClass implements Float, AtomicTypeSpecifier, CompoundTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = -2016076958464841739L;

			/**
			 * The interval range in which the {@link Float} type exists.
			 */
			private final IntervalDesignator<BigDecimal> intervalDesignator;

			/**
			 * Private constructor.
			 */
			private FloatImpl() {
				super("FLOAT");
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
				super("FLOAT");

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
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
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
			public String toString() {
//				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
				return getName();
			}
		}
	}
}
