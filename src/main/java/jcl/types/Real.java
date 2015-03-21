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
import java.math.BigInteger;

/**
 * A {@link Real} includes all numbers that represent mathematical real numbers, though there are mathematical real
 * numbers (e.g., irrational numbers) that do not have an exact representation in Common Lisp.
 * <p>
 * The types {@link Rational} and {@link Float} are disjoint subtypes of type {@link Real}.
 * <p>
 * {@link Real} -&gt; {@link Number} -&gt; {@link T}
 */
public interface Real extends Number {

	/**
	 * Singleton instance of the {@link Real} type.
	 */
	Real INSTANCE = new Factory.RealImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Real> {

		/**
		 * Gets instance of compound {@link Real} type.
		 *
		 * @param lowerBound
		 * 		the lower bound that this {@link Real} type includes
		 * @param upperBound
		 * 		the upper bound that this {@link Real} type includes
		 *
		 * @return the newly created compound {@link Real} type
		 */
		public static Real getInstance(final BigInteger lowerBound, final BigInteger upperBound) {
			return RealImpl.getInstance(lowerBound, true, upperBound, true);
		}

		/**
		 * Gets instance of compound {@link Real} type.
		 *
		 * @param lowerBound
		 * 		the lower bound that this {@link Real} type includes
		 * @param lowerInclusive
		 * 		whether to include the lower bound in the interval
		 * @param upperBound
		 * 		the upper bound that this {@link Real} type includes
		 * @param upperInclusive
		 * 		whether to include the upper bound in the interval
		 *
		 * @return the newly created compound {@link Real} type
		 */
		public static Real getInstance(final BigInteger lowerBound, final boolean lowerInclusive,
		                               final BigInteger upperBound, final boolean upperInclusive) {
			return RealImpl.getInstance(lowerBound, lowerInclusive, upperBound, upperInclusive);
		}

		/**
		 * Gets instance of compound {@link Real} type.
		 *
		 * @param lowerBound
		 * 		the lower bound that this {@link Real} type includes
		 * @param upperBound
		 * 		the upper bound that this {@link Real} type includes
		 *
		 * @return the newly created compound {@link Real} type
		 */
		public static Real getInstance(final BigDecimal lowerBound, final BigDecimal upperBound) {
			return RealImpl.getInstance(lowerBound, true, upperBound, true);
		}

		/**
		 * Gets instance of compound {@link Real} type.
		 *
		 * @param lowerBound
		 * 		the lower bound that this {@link Real} type includes
		 * @param lowerInclusive
		 * 		whether to include the lower bound in the interval
		 * @param upperBound
		 * 		the upper bound that this {@link Real} type includes
		 * @param upperInclusive
		 * 		whether to include the upper bound in the interval
		 *
		 * @return the newly created compound {@link Real} type
		 */
		public static Real getInstance(final BigDecimal lowerBound, final boolean lowerInclusive,
		                               final BigDecimal upperBound, final boolean upperInclusive) {
			return RealImpl.getInstance(lowerBound, lowerInclusive, upperBound, upperInclusive);
		}

		@Override
		public Real getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Real} type implementation.
		 */
		private static final class RealImpl extends TypeBaseClass implements Real, AtomicTypeSpecifier, CompoundTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = -1322534214307811494L;

			/**
			 * The interval range for {@link Integer} types in which the {@link Real} type exists.
			 */
			private final IntervalDesignator<BigInteger> integerIntervalDesignator;

			/**
			 * The interval range for {@link Float} types in which the {@link Real} type exists.
			 */
			private final IntervalDesignator<BigDecimal> decimalIntervalDesignator;

			/**
			 * Private constructor.
			 */
			private RealImpl() {
				super("REAL");
				integerIntervalDesignator = null;
				decimalIntervalDesignator = null;
			}

			/**
			 * Private constructor for compound {@link Real} type.
			 *
			 * @param lowerBound
			 * 		the lower bound that this {@link Real} type includes
			 * @param lowerInclusive
			 * 		whether to include the lower bound in the interval
			 * @param upperBound
			 * 		the upper bound that this {@link Real} type includes
			 * @param upperInclusive
			 * 		whether to include the upper bound in the interval
			 */
			private RealImpl(final BigInteger lowerBound, final boolean lowerInclusive,
			                 final BigInteger upperBound, final boolean upperInclusive) {
				super("REAL");

				final BigInteger realLower = lowerInclusive ? lowerBound : lowerBound.add(BigInteger.ONE);
				final BigInteger realUpper = upperInclusive ? upperBound : upperBound.subtract(BigInteger.ONE);

				integerIntervalDesignator = new IntervalDesignator<>(realLower, realUpper);
				decimalIntervalDesignator = null;
			}

			/**
			 * Private constructor for compound {@link Real} type.
			 *
			 * @param lowerBound
			 * 		the lower bound that this {@link Real} type includes
			 * @param lowerInclusive
			 * 		whether to include the lower bound in the interval
			 * @param upperBound
			 * 		the upper bound that this {@link Real} type includes
			 * @param upperInclusive
			 * 		whether to include the upper bound in the interval
			 */
			private RealImpl(final BigDecimal lowerBound, final boolean lowerInclusive,
			                 final BigDecimal upperBound, final boolean upperInclusive) {
				super("REAL");

				final BigDecimal realLower = lowerInclusive ? lowerBound : lowerBound.add(BigDecimal.ONE);
				final BigDecimal realUpper = upperInclusive ? upperBound : upperBound.subtract(BigDecimal.ONE);

				decimalIntervalDesignator = new IntervalDesignator<>(realLower, realUpper);
				integerIntervalDesignator = null;
			}

			/**
			 * Gets instance of compound {@link Real} type.
			 *
			 * @param lowerBound
			 * 		the lower bound that this {@link Real} type includes
			 * @param lowerInclusive
			 * 		whether to include the lower bound in the interval
			 * @param upperBound
			 * 		the upper bound that this {@link Real} type includes
			 * @param upperInclusive
			 * 		whether to include the upper bound in the interval
			 *
			 * @return the newly created compound {@link Real} type
			 */
			public static Real getInstance(final BigInteger lowerBound, final boolean lowerInclusive,
			                               final BigInteger upperBound, final boolean upperInclusive) {
				return new RealImpl(lowerBound, lowerInclusive, upperBound, upperInclusive);
			}

			/**
			 * Gets instance of compound {@link Real} type.
			 *
			 * @param lowerBound
			 * 		the lower bound that this {@link Real} type includes
			 * @param lowerInclusive
			 * 		whether to include the lower bound in the interval
			 * @param upperBound
			 * 		the upper bound that this {@link Real} type includes
			 * @param upperInclusive
			 * 		whether to include the upper bound in the interval
			 *
			 * @return the newly created compound {@link Real} type
			 */
			public static Real getInstance(final BigDecimal lowerBound, final boolean lowerInclusive,
			                               final BigDecimal upperBound, final boolean upperInclusive) {
				return new RealImpl(lowerBound, lowerInclusive, upperBound, upperInclusive);
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

				if (!(obj instanceof Real)) {
					return false;
				}

				final Real real = (Real) obj;
				return (real == INSTANCE) || ((real instanceof RealImpl) && checkRealImplEquality((RealImpl) real));
			}

			/**
			 * This method checks the equality of the provide realImpl object to this instance.
			 *
			 * @param realImpl
			 * 		the realImpl object to test for equality
			 *
			 * @return true if the realImpl object is equivalent to this instance; false otherwise
			 */
			private boolean checkRealImplEquality(final RealImpl realImpl) {
				if (integerIntervalDesignator != null) {
					return integerIntervalDesignator.equals(realImpl.integerIntervalDesignator);
				}

				return (decimalIntervalDesignator == null) || decimalIntervalDesignator.equals(realImpl.decimalIntervalDesignator);
			}

			@Override
			public String toString() {
//				return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).toString();
				return getName();
			}
		}
	}
}
