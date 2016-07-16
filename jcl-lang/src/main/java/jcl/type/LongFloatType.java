/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import java.math.BigDecimal;

import jcl.type.typespecifier.AtomicTypeSpecifier;
import jcl.type.typespecifier.CompoundTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link LongFloatType} is a {@link FloatType} type with a minimum of 50 precision bits and 8 exponent bits.
 * <p>
 * {@link LongFloatType} -&gt; {@link FloatType} -&gt; {@link RealType} -&gt; {@link NumberType} -&gt; {@link TType}
 */
public interface LongFloatType extends FloatType {

	/**
	 * Singleton instance of the {@link LongFloatType} type.
	 */
	LongFloatType INSTANCE = new Factory.LongFloatTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<LongFloatType> {

		/**
		 * Gets instance of compound {@link LongFloatType} type.
		 *
		 * @param lowerBound
		 * 		the lower bound that this {@link LongFloatType} type includes
		 * @param upperBound
		 * 		the upper bound that this {@link LongFloatType} type includes
		 *
		 * @return the newly created compound {@link LongFloatType} type
		 */
		public static LongFloatType getInstance(final BigDecimal lowerBound, final BigDecimal upperBound) {
			return LongFloatTypeImpl.getInstance(lowerBound, true, upperBound, true);
		}

		/**
		 * Gets instance of compound {@link LongFloatType} type.
		 *
		 * @param lowerBound
		 * 		the lower bound that this {@link LongFloatType} type includes
		 * @param lowerInclusive
		 * 		whether to include the lower bound in the interval
		 * @param upperBound
		 * 		the upper bound that this {@link LongFloatType} type includes
		 * @param upperInclusive
		 * 		whether to include the upper bound in the interval
		 *
		 * @return the newly created compound {@link LongFloatType} type
		 */
		public static LongFloatType getInstance(final BigDecimal lowerBound, final boolean lowerInclusive,
		                                        final BigDecimal upperBound, final boolean upperInclusive) {
			return LongFloatTypeImpl.getInstance(lowerBound, lowerInclusive, upperBound, upperInclusive);
		}

		@Override
		public LongFloatType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link LongFloatType} type implementation.
		 */
		private static final class LongFloatTypeImpl extends TypeBaseClass implements LongFloatType, AtomicTypeSpecifier, CompoundTypeSpecifier {

			/**
			 * The interval range in which the {@link LongFloatType} type exists.
			 */
			private final IntervalDesignator<BigDecimal> intervalDesignator;

			/**
			 * Private constructor.
			 */
			private LongFloatTypeImpl() {
				super("LONG-FLOAT");
				intervalDesignator = null;
			}

			/**
			 * Private constructor for compound {@link LongFloatType} type.
			 *
			 * @param lowerBound
			 * 		the lower bound that this {@link LongFloatType} type includes
			 * @param lowerInclusive
			 * 		whether to include the lower bound in the interval
			 * @param upperBound
			 * 		the upper bound that this {@link LongFloatType} type includes
			 * @param upperInclusive
			 * 		whether to include the upper bound in the interval
			 */
			private LongFloatTypeImpl(final BigDecimal lowerBound, final boolean lowerInclusive,
			                          final BigDecimal upperBound, final boolean upperInclusive) {
				super("LONG-FLOAT");

				final BigDecimal realLower = lowerInclusive ? lowerBound : lowerBound.add(BigDecimal.ONE);
				final BigDecimal realUpper = upperInclusive ? upperBound : upperBound.subtract(BigDecimal.ONE);
				intervalDesignator = new IntervalDesignator<>(realLower, realUpper);
			}

			/**
			 * Gets instance of compound {@link LongFloatType} type.
			 *
			 * @param lowerBound
			 * 		the lower bound that this {@link LongFloatType} type includes
			 * @param lowerInclusive
			 * 		whether to include the lower bound in the interval
			 * @param upperBound
			 * 		the upper bound that this {@link LongFloatType} type includes
			 * @param upperInclusive
			 * 		whether to include the upper bound in the interval
			 *
			 * @return the newly created compound {@link LongFloatType} type
			 */
			public static LongFloatType getInstance(final BigDecimal lowerBound, final boolean lowerInclusive,
			                                        final BigDecimal upperBound, final boolean upperInclusive) {
				return new LongFloatTypeImpl(lowerBound, lowerInclusive, upperBound, upperInclusive);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .append(intervalDesignator)
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof LongFloatType)) {
					return false;
				}

				final LongFloatType longFloatType = (LongFloatType) obj;
				if (longFloatType == INSTANCE) {
					return true;
				}

				if (longFloatType instanceof LongFloatTypeImpl) {
					final LongFloatTypeImpl longFloatTypeImpl = (LongFloatTypeImpl) longFloatType;

					return (intervalDesignator == null) || intervalDesignator.equals(longFloatTypeImpl.intervalDesignator);
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
