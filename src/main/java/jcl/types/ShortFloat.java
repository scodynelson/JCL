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
 * A {@link ShortFloat} is a {@link Float} type with a minimum of 13 precision bits and 5 exponent bits.
 * <p>
 * {@link ShortFloat} -&gt; {@link Float} -&gt; {@link Real} -&gt; {@link Number} -&gt; {@link T}
 */
public interface ShortFloat extends Float {

	/**
	 * Singleton instance of the {@link ShortFloat} type.
	 */
	ShortFloat INSTANCE = new Factory.ShortFloatImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<ShortFloat> {

		/**
		 * Gets instance of compound {@link ShortFloat} type.
		 *
		 * @param lowerBound
		 * 		the lower bound that this {@link ShortFloat} type includes
		 * @param upperBound
		 * 		the upper bound that this {@link ShortFloat} type includes
		 *
		 * @return the newly created compound {@link ShortFloat} type
		 */
		public static ShortFloat getInstance(final BigDecimal lowerBound, final BigDecimal upperBound) {
			return ShortFloatImpl.getInstance(lowerBound, true, upperBound, true);
		}

		/**
		 * Gets instance of compound {@link ShortFloat} type.
		 *
		 * @param lowerBound
		 * 		the lower bound that this {@link ShortFloat} type includes
		 * @param lowerInclusive
		 * 		whether to include the lower bound in the interval
		 * @param upperBound
		 * 		the upper bound that this {@link ShortFloat} type includes
		 * @param upperInclusive
		 * 		whether to include the upper bound in the interval
		 *
		 * @return the newly created compound {@link ShortFloat} type
		 */
		public static ShortFloat getInstance(final BigDecimal lowerBound, final boolean lowerInclusive,
		                                     final BigDecimal upperBound, final boolean upperInclusive) {
			return ShortFloatImpl.getInstance(lowerBound, lowerInclusive, upperBound, upperInclusive);
		}

		@Override
		public ShortFloat getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link ShortFloat} type implementation.
		 */
		private static final class ShortFloatImpl extends TypeBaseClass implements ShortFloat, AtomicTypeSpecifier, CompoundTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 6688479128158831477L;

			/**
			 * The interval range in which the {@link ShortFloat} type exists.
			 */
			private final IntervalDesignator<BigDecimal> intervalDesignator;

			/**
			 * Private constructor.
			 */
			private ShortFloatImpl() {
				super("SHORT-FLOAT");
				intervalDesignator = null;
			}

			/**
			 * Private constructor for compound {@link ShortFloat} type.
			 *
			 * @param lowerBound
			 * 		the lower bound that this {@link ShortFloat} type includes
			 * @param lowerInclusive
			 * 		whether to include the lower bound in the interval
			 * @param upperBound
			 * 		the upper bound that this {@link ShortFloat} type includes
			 * @param upperInclusive
			 * 		whether to include the upper bound in the interval
			 */
			private ShortFloatImpl(final BigDecimal lowerBound, final boolean lowerInclusive,
			                       final BigDecimal upperBound, final boolean upperInclusive) {
				super("SHORT-FLOAT");

				final BigDecimal realLower = lowerInclusive ? lowerBound : lowerBound.add(BigDecimal.ONE);
				final BigDecimal realUpper = upperInclusive ? upperBound : upperBound.subtract(BigDecimal.ONE);
				intervalDesignator = new IntervalDesignator<>(realLower, realUpper);
			}

			/**
			 * Gets instance of compound {@link ShortFloat} type.
			 *
			 * @param lowerBound
			 * 		the lower bound that this {@link ShortFloat} type includes
			 * @param lowerInclusive
			 * 		whether to include the lower bound in the interval
			 * @param upperBound
			 * 		the upper bound that this {@link ShortFloat} type includes
			 * @param upperInclusive
			 * 		whether to include the upper bound in the interval
			 *
			 * @return the newly created compound {@link ShortFloat} type
			 */
			public static ShortFloat getInstance(final BigDecimal lowerBound, final boolean lowerInclusive,
			                                     final BigDecimal upperBound, final boolean upperInclusive) {
				return new ShortFloatImpl(lowerBound, lowerInclusive, upperBound, upperInclusive);
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

				if (!(obj instanceof ShortFloat)) {
					return false;
				}

				final ShortFloat shortFloat = (ShortFloat) obj;
				if (shortFloat == INSTANCE) {
					return true;
				}

				if (shortFloat instanceof ShortFloatImpl) {
					final ShortFloatImpl shortFloatImpl = (ShortFloatImpl) shortFloat;

					return (intervalDesignator == null) || intervalDesignator.equals(shortFloatImpl.intervalDesignator);
				}

				return false;
			}

			@Override
			public String toString() {
				return '(' + getName() + ' ' + intervalDesignator + ')';
			}
		}
	}
}
