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
 * A {@link LongFloat} is a {@link Float} type with a minimum of 50 precision bits and 8 exponent bits.
 * <p>
 * {@link LongFloat} -&gt; {@link Float} -&gt; {@link Real} -&gt; {@link Number} -&gt; {@link T}
 */
public interface LongFloat extends Float {

	/**
	 * Singleton instance of the {@link LongFloat} type.
	 */
	LongFloat INSTANCE = new Factory.LongFloatImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<LongFloat> {

		/**
		 * Gets instance of compound {@link LongFloat} type.
		 *
		 * @param lowerBound
		 * 		the lower bound that this {@link LongFloat} type includes
		 * @param upperBound
		 * 		the upper bound that this {@link LongFloat} type includes
		 *
		 * @return the newly created compound {@link LongFloat} type
		 */
		public static LongFloat getInstance(final BigDecimal lowerBound, final BigDecimal upperBound) {
			return LongFloatImpl.getInstance(lowerBound, true, upperBound, true);
		}

		/**
		 * Gets instance of compound {@link LongFloat} type.
		 *
		 * @param lowerBound
		 * 		the lower bound that this {@link LongFloat} type includes
		 * @param lowerInclusive
		 * 		whether to include the lower bound in the interval
		 * @param upperBound
		 * 		the upper bound that this {@link LongFloat} type includes
		 * @param upperInclusive
		 * 		whether to include the upper bound in the interval
		 *
		 * @return the newly created compound {@link LongFloat} type
		 */
		public static LongFloat getInstance(final BigDecimal lowerBound, final boolean lowerInclusive,
		                                    final BigDecimal upperBound, final boolean upperInclusive) {
			return LongFloatImpl.getInstance(lowerBound, lowerInclusive, upperBound, upperInclusive);
		}

		@Override
		public LongFloat getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link LongFloat} type implementation.
		 */
		private static final class LongFloatImpl extends TypeBaseClass implements LongFloat, AtomicTypeSpecifier, CompoundTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 2113007121037284963L;

			/**
			 * The interval range in which the {@link LongFloat} type exists.
			 */
			private final IntervalDesignator<BigDecimal> intervalDesignator;

			/**
			 * Private constructor.
			 */
			private LongFloatImpl() {
				super("LONG-FLOAT");
				intervalDesignator = null;
			}

			/**
			 * Private constructor for compound {@link LongFloat} type.
			 *
			 * @param lowerBound
			 * 		the lower bound that this {@link LongFloat} type includes
			 * @param lowerInclusive
			 * 		whether to include the lower bound in the interval
			 * @param upperBound
			 * 		the upper bound that this {@link LongFloat} type includes
			 * @param upperInclusive
			 * 		whether to include the upper bound in the interval
			 */
			private LongFloatImpl(final BigDecimal lowerBound, final boolean lowerInclusive,
			                      final BigDecimal upperBound, final boolean upperInclusive) {
				super("LONG-FLOAT");

				final BigDecimal realLower = lowerInclusive ? lowerBound : lowerBound.add(BigDecimal.ONE);
				final BigDecimal realUpper = upperInclusive ? upperBound : upperBound.subtract(BigDecimal.ONE);
				intervalDesignator = new IntervalDesignator<>(realLower, realUpper);
			}

			/**
			 * Gets instance of compound {@link LongFloat} type.
			 *
			 * @param lowerBound
			 * 		the lower bound that this {@link LongFloat} type includes
			 * @param lowerInclusive
			 * 		whether to include the lower bound in the interval
			 * @param upperBound
			 * 		the upper bound that this {@link LongFloat} type includes
			 * @param upperInclusive
			 * 		whether to include the upper bound in the interval
			 *
			 * @return the newly created compound {@link LongFloat} type
			 */
			public static LongFloat getInstance(final BigDecimal lowerBound, final boolean lowerInclusive,
			                                    final BigDecimal upperBound, final boolean upperInclusive) {
				return new LongFloatImpl(lowerBound, lowerInclusive, upperBound, upperInclusive);
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
			public String toString() {
//				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
				return getName();
			}
		}
	}
}
