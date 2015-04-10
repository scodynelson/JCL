/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import java.math.BigDecimal;
import java.math.BigInteger;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.CompoundTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link RealType} includes all numbers that represent mathematical real numbers, though there are mathematical real
 * numbers (e.g., irrational numbers) that do not have an exact representation in Common Lisp.
 * <p>
 * The types {@link RationalType} and {@link FloatType} are disjoint subtypes of type {@link RealType}.
 * <p>
 * {@link RealType} -&gt; {@link NumberType} -&gt; {@link TType}
 */
public interface RealType extends NumberType {

	/**
	 * Singleton instance of the {@link RealType} type.
	 */
	RealType INSTANCE = new Factory.RealTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<RealType> {

		/**
		 * Gets instance of compound {@link RealType} type.
		 *
		 * @param lowerBound
		 * 		the lower bound that this {@link RealType} type includes
		 * @param upperBound
		 * 		the upper bound that this {@link RealType} type includes
		 *
		 * @return the newly created compound {@link RealType} type
		 */
		public static RealType getInstance(final BigInteger lowerBound, final BigInteger upperBound) {
			return RealTypeImpl.getInstance(lowerBound, true, upperBound, true);
		}

		/**
		 * Gets instance of compound {@link RealType} type.
		 *
		 * @param lowerBound
		 * 		the lower bound that this {@link RealType} type includes
		 * @param lowerInclusive
		 * 		whether to include the lower bound in the interval
		 * @param upperBound
		 * 		the upper bound that this {@link RealType} type includes
		 * @param upperInclusive
		 * 		whether to include the upper bound in the interval
		 *
		 * @return the newly created compound {@link RealType} type
		 */
		public static RealType getInstance(final BigInteger lowerBound, final boolean lowerInclusive,
		                                   final BigInteger upperBound, final boolean upperInclusive) {
			return RealTypeImpl.getInstance(lowerBound, lowerInclusive, upperBound, upperInclusive);
		}

		/**
		 * Gets instance of compound {@link RealType} type.
		 *
		 * @param lowerBound
		 * 		the lower bound that this {@link RealType} type includes
		 * @param upperBound
		 * 		the upper bound that this {@link RealType} type includes
		 *
		 * @return the newly created compound {@link RealType} type
		 */
		public static RealType getInstance(final BigDecimal lowerBound, final BigDecimal upperBound) {
			return RealTypeImpl.getInstance(lowerBound, true, upperBound, true);
		}

		/**
		 * Gets instance of compound {@link RealType} type.
		 *
		 * @param lowerBound
		 * 		the lower bound that this {@link RealType} type includes
		 * @param lowerInclusive
		 * 		whether to include the lower bound in the interval
		 * @param upperBound
		 * 		the upper bound that this {@link RealType} type includes
		 * @param upperInclusive
		 * 		whether to include the upper bound in the interval
		 *
		 * @return the newly created compound {@link RealType} type
		 */
		public static RealType getInstance(final BigDecimal lowerBound, final boolean lowerInclusive,
		                                   final BigDecimal upperBound, final boolean upperInclusive) {
			return RealTypeImpl.getInstance(lowerBound, lowerInclusive, upperBound, upperInclusive);
		}

		@Override
		public RealType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link RealType} type implementation.
		 */
		private static final class RealTypeImpl extends TypeBaseClass implements RealType, AtomicTypeSpecifier, CompoundTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = -1322534214307811494L;

			/**
			 * The interval range for {@link IntegerType} types in which the {@link RealType} type exists.
			 */
			private final IntervalDesignator<BigInteger> integerIntervalDesignator;

			/**
			 * The interval range for {@link FloatType} types in which the {@link RealType} type exists.
			 */
			private final IntervalDesignator<BigDecimal> decimalIntervalDesignator;

			/**
			 * Private constructor.
			 */
			private RealTypeImpl() {
				super("REAL");
				integerIntervalDesignator = null;
				decimalIntervalDesignator = null;
			}

			/**
			 * Private constructor for compound {@link RealType} type.
			 *
			 * @param lowerBound
			 * 		the lower bound that this {@link RealType} type includes
			 * @param lowerInclusive
			 * 		whether to include the lower bound in the interval
			 * @param upperBound
			 * 		the upper bound that this {@link RealType} type includes
			 * @param upperInclusive
			 * 		whether to include the upper bound in the interval
			 */
			private RealTypeImpl(final BigInteger lowerBound, final boolean lowerInclusive,
			                     final BigInteger upperBound, final boolean upperInclusive) {
				super("REAL");

				final BigInteger realLower = lowerInclusive ? lowerBound : lowerBound.add(BigInteger.ONE);
				final BigInteger realUpper = upperInclusive ? upperBound : upperBound.subtract(BigInteger.ONE);

				integerIntervalDesignator = new IntervalDesignator<>(realLower, realUpper);
				decimalIntervalDesignator = null;
			}

			/**
			 * Private constructor for compound {@link RealType} type.
			 *
			 * @param lowerBound
			 * 		the lower bound that this {@link RealType} type includes
			 * @param lowerInclusive
			 * 		whether to include the lower bound in the interval
			 * @param upperBound
			 * 		the upper bound that this {@link RealType} type includes
			 * @param upperInclusive
			 * 		whether to include the upper bound in the interval
			 */
			private RealTypeImpl(final BigDecimal lowerBound, final boolean lowerInclusive,
			                     final BigDecimal upperBound, final boolean upperInclusive) {
				super("REAL");

				final BigDecimal realLower = lowerInclusive ? lowerBound : lowerBound.add(BigDecimal.ONE);
				final BigDecimal realUpper = upperInclusive ? upperBound : upperBound.subtract(BigDecimal.ONE);

				decimalIntervalDesignator = new IntervalDesignator<>(realLower, realUpper);
				integerIntervalDesignator = null;
			}

			/**
			 * Gets instance of compound {@link RealType} type.
			 *
			 * @param lowerBound
			 * 		the lower bound that this {@link RealType} type includes
			 * @param lowerInclusive
			 * 		whether to include the lower bound in the interval
			 * @param upperBound
			 * 		the upper bound that this {@link RealType} type includes
			 * @param upperInclusive
			 * 		whether to include the upper bound in the interval
			 *
			 * @return the newly created compound {@link RealType} type
			 */
			public static RealType getInstance(final BigInteger lowerBound, final boolean lowerInclusive,
			                                   final BigInteger upperBound, final boolean upperInclusive) {
				return new RealTypeImpl(lowerBound, lowerInclusive, upperBound, upperInclusive);
			}

			/**
			 * Gets instance of compound {@link RealType} type.
			 *
			 * @param lowerBound
			 * 		the lower bound that this {@link RealType} type includes
			 * @param lowerInclusive
			 * 		whether to include the lower bound in the interval
			 * @param upperBound
			 * 		the upper bound that this {@link RealType} type includes
			 * @param upperInclusive
			 * 		whether to include the upper bound in the interval
			 *
			 * @return the newly created compound {@link RealType} type
			 */
			public static RealType getInstance(final BigDecimal lowerBound, final boolean lowerInclusive,
			                                   final BigDecimal upperBound, final boolean upperInclusive) {
				return new RealTypeImpl(lowerBound, lowerInclusive, upperBound, upperInclusive);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .append(integerIntervalDesignator)
				                            .append(decimalIntervalDesignator)
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof RealType)) {
					return false;
				}

				final RealType realType = (RealType) obj;
				if (realType == INSTANCE) {
					return true;
				}

				if (realType instanceof RealTypeImpl) {
					final RealTypeImpl realTypeImpl = (RealTypeImpl) realType;

					return checkRealTypeImplEquality(realTypeImpl);
				}

				// This means it is a subtype, and thus is equal.
				return true;
			}

			/**
			 * This method checks the equality of the provided {@code realTypeImpl} object to this instance.
			 *
			 * @param realTypeImpl
			 * 		the RealTypeImpl object to test for equality
			 *
			 * @return true if the {@code realTypeImpl} object is equivalent to this instance; false otherwise
			 */
			private boolean checkRealTypeImplEquality(final RealTypeImpl realTypeImpl) {
				if (integerIntervalDesignator != null) {
					return integerIntervalDesignator.equals(realTypeImpl.integerIntervalDesignator);
				}

				return (decimalIntervalDesignator == null) || decimalIntervalDesignator.equals(realTypeImpl.decimalIntervalDesignator);
			}

			@Override
			public String toString() {
				if (integerIntervalDesignator != null) {
					return '(' + getName() + ' ' + integerIntervalDesignator + ')';
				} else if (decimalIntervalDesignator != null) {
					return '(' + getName() + ' ' + decimalIntervalDesignator + ')';
				} else {
					return '(' + getName() + ' ' + '*' + ')';
				}
			}
		}
	}
}
