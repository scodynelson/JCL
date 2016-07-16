/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import java.math.BigDecimal;

import jcl.type.typespecifier.AtomicTypeSpecifier;
import jcl.type.typespecifier.CompoundTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link ShortFloatType} is a {@link FloatType} type with a minimum of 13 precision bits and 5 exponent bits.
 * <p>
 * {@link ShortFloatType} -&gt; {@link FloatType} -&gt; {@link RealType} -&gt; {@link NumberType} -&gt; {@link TType}
 */
public interface ShortFloatType extends FloatType {

	/**
	 * Singleton instance of the {@link ShortFloatType} type.
	 */
	ShortFloatType INSTANCE = new Factory.ShortFloatTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<ShortFloatType> {

		/**
		 * Gets instance of compound {@link ShortFloatType} type.
		 *
		 * @param lowerBound
		 * 		the lower bound that this {@link ShortFloatType} type includes
		 * @param upperBound
		 * 		the upper bound that this {@link ShortFloatType} type includes
		 *
		 * @return the newly created compound {@link ShortFloatType} type
		 */
		public static ShortFloatType getInstance(final BigDecimal lowerBound, final BigDecimal upperBound) {
			return ShortFloatTypeImpl.getInstance(lowerBound, true, upperBound, true);
		}

		/**
		 * Gets instance of compound {@link ShortFloatType} type.
		 *
		 * @param lowerBound
		 * 		the lower bound that this {@link ShortFloatType} type includes
		 * @param lowerInclusive
		 * 		whether to include the lower bound in the interval
		 * @param upperBound
		 * 		the upper bound that this {@link ShortFloatType} type includes
		 * @param upperInclusive
		 * 		whether to include the upper bound in the interval
		 *
		 * @return the newly created compound {@link ShortFloatType} type
		 */
		public static ShortFloatType getInstance(final BigDecimal lowerBound, final boolean lowerInclusive,
		                                         final BigDecimal upperBound, final boolean upperInclusive) {
			return ShortFloatTypeImpl.getInstance(lowerBound, lowerInclusive, upperBound, upperInclusive);
		}

		@Override
		public ShortFloatType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link ShortFloatType} type implementation.
		 */
		private static final class ShortFloatTypeImpl extends TypeBaseClass implements ShortFloatType, AtomicTypeSpecifier, CompoundTypeSpecifier {

			/**
			 * The interval range in which the {@link ShortFloatType} type exists.
			 */
			private final IntervalDesignator<BigDecimal> intervalDesignator;

			/**
			 * Private constructor.
			 */
			private ShortFloatTypeImpl() {
				super("SHORT-FLOAT");
				intervalDesignator = null;
			}

			/**
			 * Private constructor for compound {@link ShortFloatType} type.
			 *
			 * @param lowerBound
			 * 		the lower bound that this {@link ShortFloatType} type includes
			 * @param lowerInclusive
			 * 		whether to include the lower bound in the interval
			 * @param upperBound
			 * 		the upper bound that this {@link ShortFloatType} type includes
			 * @param upperInclusive
			 * 		whether to include the upper bound in the interval
			 */
			private ShortFloatTypeImpl(final BigDecimal lowerBound, final boolean lowerInclusive,
			                           final BigDecimal upperBound, final boolean upperInclusive) {
				super("SHORT-FLOAT");

				final BigDecimal realLower = lowerInclusive ? lowerBound : lowerBound.add(BigDecimal.ONE);
				final BigDecimal realUpper = upperInclusive ? upperBound : upperBound.subtract(BigDecimal.ONE);
				intervalDesignator = new IntervalDesignator<>(realLower, realUpper);
			}

			/**
			 * Gets instance of compound {@link ShortFloatType} type.
			 *
			 * @param lowerBound
			 * 		the lower bound that this {@link ShortFloatType} type includes
			 * @param lowerInclusive
			 * 		whether to include the lower bound in the interval
			 * @param upperBound
			 * 		the upper bound that this {@link ShortFloatType} type includes
			 * @param upperInclusive
			 * 		whether to include the upper bound in the interval
			 *
			 * @return the newly created compound {@link ShortFloatType} type
			 */
			public static ShortFloatType getInstance(final BigDecimal lowerBound, final boolean lowerInclusive,
			                                         final BigDecimal upperBound, final boolean upperInclusive) {
				return new ShortFloatTypeImpl(lowerBound, lowerInclusive, upperBound, upperInclusive);
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

				if (!(obj instanceof ShortFloatType)) {
					return false;
				}

				final ShortFloatType shortFloatType = (ShortFloatType) obj;
				if (shortFloatType == INSTANCE) {
					return true;
				}

				if (shortFloatType instanceof ShortFloatTypeImpl) {
					final ShortFloatTypeImpl shortFloatTypeImpl = (ShortFloatTypeImpl) shortFloatType;

					return (intervalDesignator == null) || intervalDesignator.equals(shortFloatTypeImpl.intervalDesignator);
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
