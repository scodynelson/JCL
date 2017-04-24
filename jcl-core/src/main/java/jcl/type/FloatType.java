/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import java.math.BigDecimal;

import jcl.type.typespecifier.AtomicTypeSpecifier;
import jcl.type.typespecifier.CompoundTypeSpecifier;

/**
 * A {@link FloatType} is a mathematical rational (but not a Common Lisp {@link RationalType}) of the form s*f*b^e-p,
 * where s is +1 or -1, the sign; b is an {@link IntegerType} greater than 1, the base or radix of the representation;
 * p is a positive {@link IntegerType}, the precision (in base-b digits) of the {@link FloatType}; f is a positive
 * {@link IntegerType} between b^p-1 and b^p-1 (inclusive), the significand; and e is an {@link IntegerType}, the
 * exponent.
 * <p>
 * The types {@link ShortFloatType}, {@link SingleFloatType}, {@link DoubleFloatType}, and {@link LongFloatType} are
 * subtypes of type {@link FloatType}.
 * <p>
 * {@link FloatType} -&gt; {@link RealType} -&gt; {@link NumberType} -&gt; {@link TType}
 */
public interface FloatType extends RealType {

	/**
	 * Singleton instance of the {@link FloatType} type.
	 */
	FloatType INSTANCE = new Factory.FloatTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<FloatType> {

		/**
		 * Gets instance of compound {@link FloatType} type.
		 *
		 * @param lowerBound
		 * 		the lower bound that this {@link FloatType} type includes
		 * @param upperBound
		 * 		the upper bound that this {@link FloatType} type includes
		 *
		 * @return the newly created compound {@link FloatType} type
		 */
		public static FloatType getInstance(final BigDecimal lowerBound, final BigDecimal upperBound) {
			return FloatTypeImpl.getInstance(lowerBound, true, upperBound, true);
		}

		/**
		 * Gets instance of compound {@link FloatType} type.
		 *
		 * @param lowerBound
		 * 		the lower bound that this {@link FloatType} type includes
		 * @param lowerInclusive
		 * 		whether to include the lower bound in the interval
		 * @param upperBound
		 * 		the upper bound that this {@link FloatType} type includes
		 * @param upperInclusive
		 * 		whether to include the upper bound in the interval
		 *
		 * @return the newly created compound {@link FloatType} type
		 */
		public static FloatType getInstance(final BigDecimal lowerBound, final boolean lowerInclusive,
		                                    final BigDecimal upperBound, final boolean upperInclusive) {
			return FloatTypeImpl.getInstance(lowerBound, lowerInclusive, upperBound, upperInclusive);
		}

		@Override
		public FloatType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link FloatType} type implementation.
		 */
		private static final class FloatTypeImpl extends TypeBaseClass implements FloatType, AtomicTypeSpecifier, CompoundTypeSpecifier {

			/**
			 * The interval range in which the {@link FloatType} type exists.
			 */
			private final IntervalDesignator<BigDecimal> intervalDesignator;

			/**
			 * Private constructor.
			 */
			private FloatTypeImpl() {
				super("FLOAT");
				intervalDesignator = null;
			}

			/**
			 * Private constructor for compound {@link FloatType} type.
			 *
			 * @param lowerBound
			 * 		the lower bound that this {@link FloatType} type includes
			 * @param lowerInclusive
			 * 		whether to include the lower bound in the interval
			 * @param upperBound
			 * 		the upper bound that this {@link FloatType} type includes
			 * @param upperInclusive
			 * 		whether to include the upper bound in the interval
			 */
			private FloatTypeImpl(final BigDecimal lowerBound, final boolean lowerInclusive,
			                      final BigDecimal upperBound, final boolean upperInclusive) {
				super("FLOAT");

				final BigDecimal realLower = lowerInclusive ? lowerBound : lowerBound.add(BigDecimal.ONE);
				final BigDecimal realUpper = upperInclusive ? upperBound : upperBound.subtract(BigDecimal.ONE);
				intervalDesignator = new IntervalDesignator<>(realLower, realUpper);
			}

			/**
			 * Gets instance of compound {@link FloatType} type.
			 *
			 * @param lowerBound
			 * 		the lower bound that this {@link FloatType} type includes
			 * @param lowerInclusive
			 * 		whether to include the lower bound in the interval
			 * @param upperBound
			 * 		the upper bound that this {@link FloatType} type includes
			 * @param upperInclusive
			 * 		whether to include the upper bound in the interval
			 *
			 * @return the newly created compound {@link FloatType} type
			 */
			public static FloatType getInstance(final BigDecimal lowerBound, final boolean lowerInclusive,
			                                    final BigDecimal upperBound, final boolean upperInclusive) {
				return new FloatTypeImpl(lowerBound, lowerInclusive, upperBound, upperInclusive);
			}

			@Override
			public boolean typeEquals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof FloatType)) {
					return false;
				}

				final FloatType floatType = (FloatType) obj;
				if (floatType == INSTANCE) {
					return true;
				}

				if (floatType instanceof FloatTypeImpl) {
					final FloatTypeImpl floatTypeImpl = (FloatTypeImpl) floatType;

					return (intervalDesignator == null) || intervalDesignator.equals(floatTypeImpl.intervalDesignator);
				}

				// This means it is a subtype, and thus is equal.
				return true;
			}

			@Override
			public String toString() {
				return '(' + getName() + ' ' + intervalDesignator + ')';
			}
		}
	}
}
