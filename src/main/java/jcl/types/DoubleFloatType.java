/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import java.math.BigDecimal;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.CompoundTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link DoubleFloatType} is a {@link FloatType} type with a minimum of 50 precision bits and 8 exponent bits.
 * <p>
 * {@link DoubleFloatType} -&gt; {@link FloatType} -&gt; {@link RealType} -&gt; {@link NumberType} -&gt; {@link TType}
 */
public interface DoubleFloatType extends FloatType {

	/**
	 * Singleton instance of the {@link DoubleFloatType} type.
	 */
	DoubleFloatType INSTANCE = new Factory.DoubleFloatTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<DoubleFloatType> {

		/**
		 * Gets instance of compound {@link DoubleFloatType} type.
		 *
		 * @param lowerBound
		 * 		the lower bound that this {@link DoubleFloatType} type includes
		 * @param upperBound
		 * 		the upper bound that this {@link DoubleFloatType} type includes
		 *
		 * @return the newly created compound {@link DoubleFloatType} type
		 */
		public static DoubleFloatType getInstance(final BigDecimal lowerBound, final BigDecimal upperBound) {
			return DoubleFloatTypeImpl.getInstance(lowerBound, true, upperBound, true);
		}

		/**
		 * Gets instance of compound {@link DoubleFloatType} type.
		 *
		 * @param lowerBound
		 * 		the lower bound that this {@link DoubleFloatType} type includes
		 * @param lowerInclusive
		 * 		whether to include the lower bound in the interval
		 * @param upperBound
		 * 		the upper bound that this {@link DoubleFloatType} type includes
		 * @param upperInclusive
		 * 		whether to include the upper bound in the interval
		 *
		 * @return the newly created compound {@link DoubleFloatType} type
		 */
		public static DoubleFloatType getInstance(final BigDecimal lowerBound, final boolean lowerInclusive,
		                                          final BigDecimal upperBound, final boolean upperInclusive) {
			return DoubleFloatTypeImpl.getInstance(lowerBound, lowerInclusive, upperBound, upperInclusive);
		}

		@Override
		public DoubleFloatType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link DoubleFloatType} type implementation.
		 */
		private static final class DoubleFloatTypeImpl extends TypeBaseClass implements DoubleFloatType, AtomicTypeSpecifier, CompoundTypeSpecifier {

			/**
			 * The interval range in which the {@link DoubleFloatType} type exists.
			 */
			private final IntervalDesignator<BigDecimal> intervalDesignator;

			/**
			 * Private constructor.
			 */
			private DoubleFloatTypeImpl() {
				super("DOUBLE-FLOAT");
				intervalDesignator = null;
			}

			/**
			 * Private constructor for compound {@link DoubleFloatType} type.
			 *
			 * @param lowerBound
			 * 		the lower bound that this {@link DoubleFloatType} type includes
			 * @param lowerInclusive
			 * 		whether to include the lower bound in the interval
			 * @param upperBound
			 * 		the upper bound that this {@link DoubleFloatType} type includes
			 * @param upperInclusive
			 * 		whether to include the upper bound in the interval
			 */
			private DoubleFloatTypeImpl(final BigDecimal lowerBound, final boolean lowerInclusive,
			                            final BigDecimal upperBound, final boolean upperInclusive) {
				super("DOUBLE-FLOAT");

				final BigDecimal realLower = lowerInclusive ? lowerBound : lowerBound.add(BigDecimal.ONE);
				final BigDecimal realUpper = upperInclusive ? upperBound : upperBound.subtract(BigDecimal.ONE);
				intervalDesignator = new IntervalDesignator<>(realLower, realUpper);
			}

			/**
			 * Gets instance of compound {@link DoubleFloatType} type.
			 *
			 * @param lowerBound
			 * 		the lower bound that this {@link DoubleFloatType} type includes
			 * @param lowerInclusive
			 * 		whether to include the lower bound in the interval
			 * @param upperBound
			 * 		the upper bound that this {@link DoubleFloatType} type includes
			 * @param upperInclusive
			 * 		whether to include the upper bound in the interval
			 *
			 * @return the newly created compound {@link DoubleFloatType} type
			 */
			public static DoubleFloatType getInstance(final BigDecimal lowerBound, final boolean lowerInclusive,
			                                          final BigDecimal upperBound, final boolean upperInclusive) {
				return new DoubleFloatTypeImpl(lowerBound, lowerInclusive, upperBound, upperInclusive);
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

				if (!(obj instanceof DoubleFloatType)) {
					return false;
				}

				final DoubleFloatType doubleFloatType = (DoubleFloatType) obj;
				if (doubleFloatType == INSTANCE) {
					return true;
				}

				if (doubleFloatType instanceof DoubleFloatTypeImpl) {
					final DoubleFloatTypeImpl doubleFloatTypeImpl = (DoubleFloatTypeImpl) doubleFloatType;

					return (intervalDesignator == null) || intervalDesignator.equals(doubleFloatTypeImpl.intervalDesignator);
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
