/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import java.math.BigDecimal;

import jcl.type.typespecifier.AtomicTypeSpecifier;
import jcl.type.typespecifier.CompoundTypeSpecifier;

/**
 * A {@link SingleFloatType} is a {@link FloatType} type with a minimum of 24 precision bits and 8 exponent bits.
 * <p>
 * {@link SingleFloatType} -&gt; {@link FloatType} -&gt; {@link RealType} -&gt; {@link NumberType} -&gt; {@link TType}
 */
public interface SingleFloatType extends FloatType {

	/**
	 * Singleton instance of the {@link SingleFloatType} type.
	 */
	SingleFloatType INSTANCE = new Factory.SingleFloatTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<SingleFloatType> {

		/**
		 * Gets instance of compound {@link SingleFloatType} type.
		 *
		 * @param lowerBound
		 * 		the lower bound that this {@link SingleFloatType} type includes
		 * @param upperBound
		 * 		the upper bound that this {@link SingleFloatType} type includes
		 *
		 * @return the newly created compound {@link SingleFloatType} type
		 */
		public static SingleFloatType getInstance(final BigDecimal lowerBound, final BigDecimal upperBound) {
			return SingleFloatTypeImpl.getInstance(lowerBound, true, upperBound, true);
		}

		/**
		 * Gets instance of compound {@link SingleFloatType} type.
		 *
		 * @param lowerBound
		 * 		the lower bound that this {@link SingleFloatType} type includes
		 * @param lowerInclusive
		 * 		whether to include the lower bound in the interval
		 * @param upperBound
		 * 		the upper bound that this {@link SingleFloatType} type includes
		 * @param upperInclusive
		 * 		whether to include the upper bound in the interval
		 *
		 * @return the newly created compound {@link SingleFloatType} type
		 */
		public static SingleFloatType getInstance(final BigDecimal lowerBound, final boolean lowerInclusive,
		                                          final BigDecimal upperBound, final boolean upperInclusive) {
			return SingleFloatTypeImpl.getInstance(lowerBound, lowerInclusive, upperBound, upperInclusive);
		}

		@Override
		public SingleFloatType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link SingleFloatType} type implementation.
		 */
		private static final class SingleFloatTypeImpl extends TypeBaseClass implements SingleFloatType, AtomicTypeSpecifier, CompoundTypeSpecifier {

			/**
			 * The interval range in which the {@link SingleFloatType} type exists.
			 */
			private final IntervalDesignator<BigDecimal> intervalDesignator;

			/**
			 * Private constructor.
			 */
			private SingleFloatTypeImpl() {
				super("SINGLE-FLOAT");
				intervalDesignator = null;
			}

			/**
			 * Private constructor for compound {@link SingleFloatType} type.
			 *
			 * @param lowerBound
			 * 		the lower bound that this {@link SingleFloatType} type includes
			 * @param lowerInclusive
			 * 		whether to include the lower bound in the interval
			 * @param upperBound
			 * 		the upper bound that this {@link SingleFloatType} type includes
			 * @param upperInclusive
			 * 		whether to include the upper bound in the interval
			 */
			private SingleFloatTypeImpl(final BigDecimal lowerBound, final boolean lowerInclusive,
			                            final BigDecimal upperBound, final boolean upperInclusive) {
				super("SINGLE-FLOAT");

				final BigDecimal realLower = lowerInclusive ? lowerBound : lowerBound.add(BigDecimal.ONE);
				final BigDecimal realUpper = upperInclusive ? upperBound : upperBound.subtract(BigDecimal.ONE);
				intervalDesignator = new IntervalDesignator<>(realLower, realUpper);
			}

			/**
			 * Gets instance of compound {@link SingleFloatType} type.
			 *
			 * @param lowerBound
			 * 		the lower bound that this {@link SingleFloatType} type includes
			 * @param lowerInclusive
			 * 		whether to include the lower bound in the interval
			 * @param upperBound
			 * 		the upper bound that this {@link SingleFloatType} type includes
			 * @param upperInclusive
			 * 		whether to include the upper bound in the interval
			 *
			 * @return the newly created compound {@link SingleFloatType} type
			 */
			public static SingleFloatType getInstance(final BigDecimal lowerBound, final boolean lowerInclusive,
			                                          final BigDecimal upperBound, final boolean upperInclusive) {
				return new SingleFloatTypeImpl(lowerBound, lowerInclusive, upperBound, upperInclusive);
			}

			@Override
			public boolean typeEquals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof SingleFloatType)) {
					return false;
				}

				final SingleFloatType singleFloatType = (SingleFloatType) obj;
				if (singleFloatType == INSTANCE) {
					return true;
				}

				if (singleFloatType instanceof SingleFloatTypeImpl) {
					final SingleFloatTypeImpl singleFloatTypeImpl = (SingleFloatTypeImpl) singleFloatType;

					return (intervalDesignator == null) || intervalDesignator.equals(singleFloatTypeImpl.intervalDesignator);
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
