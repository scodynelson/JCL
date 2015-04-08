/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import java.lang.String;
import java.math.BigDecimal;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.CompoundTypeSpecifier;
import jcl.types.typespecifiers.designator.IntervalDesignator;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link DoubleFloat} is a {@link Float} type with a minimum of 50 precision bits and 8 exponent bits.
 * <p>
 * {@link DoubleFloat} -&gt; {@link Float} -&gt; {@link Real} -&gt; {@link Number} -&gt; {@link T}
 */
public interface DoubleFloat extends Float {

	/**
	 * Singleton instance of the {@link DoubleFloat} type.
	 */
	DoubleFloat INSTANCE = new Factory.DoubleFloatImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<DoubleFloat> {

		/**
		 * Gets instance of compound {@link DoubleFloat} type.
		 *
		 * @param lowerBound
		 * 		the lower bound that this {@link DoubleFloat} type includes
		 * @param upperBound
		 * 		the upper bound that this {@link DoubleFloat} type includes
		 *
		 * @return the newly created compound {@link DoubleFloat} type
		 */
		public static DoubleFloat getInstance(final BigDecimal lowerBound, final BigDecimal upperBound) {
			return DoubleFloatImpl.getInstance(lowerBound, true, upperBound, true);
		}

		/**
		 * Gets instance of compound {@link DoubleFloat} type.
		 *
		 * @param lowerBound
		 * 		the lower bound that this {@link DoubleFloat} type includes
		 * @param lowerInclusive
		 * 		whether to include the lower bound in the interval
		 * @param upperBound
		 * 		the upper bound that this {@link DoubleFloat} type includes
		 * @param upperInclusive
		 * 		whether to include the upper bound in the interval
		 *
		 * @return the newly created compound {@link DoubleFloat} type
		 */
		public static DoubleFloat getInstance(final BigDecimal lowerBound, final boolean lowerInclusive,
		                                      final BigDecimal upperBound, final boolean upperInclusive) {
			return DoubleFloatImpl.getInstance(lowerBound, lowerInclusive, upperBound, upperInclusive);
		}

		@Override
		public DoubleFloat getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link DoubleFloat} type implementation.
		 */
		private static final class DoubleFloatImpl extends TypeBaseClass implements DoubleFloat, AtomicTypeSpecifier, CompoundTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = -5453487463729956878L;

			/**
			 * The interval range in which the {@link DoubleFloat} type exists.
			 */
			private final IntervalDesignator<BigDecimal> intervalDesignator;

			/**
			 * Private constructor.
			 */
			private DoubleFloatImpl() {
				super("DOUBLE-FLOAT");
				intervalDesignator = null;
			}

			/**
			 * Private constructor for compound {@link DoubleFloat} type.
			 *
			 * @param lowerBound
			 * 		the lower bound that this {@link DoubleFloat} type includes
			 * @param lowerInclusive
			 * 		whether to include the lower bound in the interval
			 * @param upperBound
			 * 		the upper bound that this {@link DoubleFloat} type includes
			 * @param upperInclusive
			 * 		whether to include the upper bound in the interval
			 */
			private DoubleFloatImpl(final BigDecimal lowerBound, final boolean lowerInclusive,
			                        final BigDecimal upperBound, final boolean upperInclusive) {
				super("DOUBLE-FLOAT");

				final BigDecimal realLower = lowerInclusive ? lowerBound : lowerBound.add(BigDecimal.ONE);
				final BigDecimal realUpper = upperInclusive ? upperBound : upperBound.subtract(BigDecimal.ONE);
				intervalDesignator = new IntervalDesignator<>(realLower, realUpper);
			}

			/**
			 * Gets instance of compound {@link DoubleFloat} type.
			 *
			 * @param lowerBound
			 * 		the lower bound that this {@link DoubleFloat} type includes
			 * @param lowerInclusive
			 * 		whether to include the lower bound in the interval
			 * @param upperBound
			 * 		the upper bound that this {@link DoubleFloat} type includes
			 * @param upperInclusive
			 * 		whether to include the upper bound in the interval
			 *
			 * @return the newly created compound {@link DoubleFloat} type
			 */
			public static DoubleFloat getInstance(final BigDecimal lowerBound, final boolean lowerInclusive,
			                                      final BigDecimal upperBound, final boolean upperInclusive) {
				return new DoubleFloatImpl(lowerBound, lowerInclusive, upperBound, upperInclusive);
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

				if (!(obj instanceof DoubleFloat)) {
					return false;
				}

				final DoubleFloat doubleFloat = (DoubleFloat) obj;
				if (doubleFloat == INSTANCE) {
					return true;
				}

				if (doubleFloat instanceof DoubleFloatImpl) {
					final DoubleFloatImpl doubleFloatImpl = (DoubleFloatImpl) doubleFloat;

					return (intervalDesignator == null) || intervalDesignator.equals(doubleFloatImpl.intervalDesignator);
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
