/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import java.math.BigInteger;

import jcl.type.typespecifier.AtomicTypeSpecifier;
import jcl.type.typespecifier.CompoundTypeSpecifier;

/**
 * A {@link RationalType} is a {@link NumberType} with a canonical representation of an {@link IntegerType} if its
 * value is integral, and otherwise as a {@link RatioType}.
 * <p>
 * The types {@link IntegerType} and {@link RatioType} are disjoint subtypes of type {@link RationalType}.
 * <p>
 * {@link RationalType} -&gt; {@link RealType} -&gt; {@link NumberType} -&gt; {@link TType}
 */
public interface RationalType extends RealType {

	/**
	 * Singleton instance of the {@link RationalType} type.
	 */
	RationalType INSTANCE = new Factory.RationalTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<RationalType> {

		/**
		 * Gets instance of compound {@link RationalType} type.
		 *
		 * @param lowerBound
		 * 		the lower bound that this {@link RationalType} type includes
		 * @param upperBound
		 * 		the upper bound that this {@link RationalType} type includes
		 *
		 * @return the newly created compound {@link RationalType} type
		 */
		public static RationalType getInstance(final BigInteger lowerBound, final BigInteger upperBound) {
			return RationalTypeImpl.getInstance(lowerBound, true, upperBound, true);
		}

		/**
		 * Gets instance of compound {@link RationalType} type.
		 *
		 * @param lowerBound
		 * 		the lower bound that this {@link RationalType} type includes
		 * @param lowerInclusive
		 * 		whether to include the lower bound in the interval
		 * @param upperBound
		 * 		the upper bound that this {@link RationalType} type includes
		 * @param upperInclusive
		 * 		whether to include the upper bound in the interval
		 *
		 * @return the newly created compound {@link RationalType} type
		 */
		public static RationalType getInstance(final BigInteger lowerBound, final boolean lowerInclusive,
		                                       final BigInteger upperBound, final boolean upperInclusive) {
			return RationalTypeImpl.getInstance(lowerBound, lowerInclusive, upperBound, upperInclusive);
		}

		@Override
		public RationalType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link RationalType} type implementation.
		 */
		private static final class RationalTypeImpl extends TypeBaseClass implements RationalType, AtomicTypeSpecifier, CompoundTypeSpecifier {

			/**
			 * The interval range in which the {@link RationalType} type exists.
			 */
			private final IntervalDesignator<BigInteger> intervalDesignator;

			/**
			 * Private constructor.
			 */
			private RationalTypeImpl() {
				super("RATIONAL");
				intervalDesignator = null;
			}

			/**
			 * Private constructor for compound {@link RationalType} type.
			 *
			 * @param lowerBound
			 * 		the lower bound that this {@link RationalType} type includes
			 * @param lowerInclusive
			 * 		whether to include the lower bound in the interval
			 * @param upperBound
			 * 		the upper bound that this {@link RationalType} type includes
			 * @param upperInclusive
			 * 		whether to include the upper bound in the interval
			 */
			private RationalTypeImpl(final BigInteger lowerBound, final boolean lowerInclusive,
			                         final BigInteger upperBound, final boolean upperInclusive) {
				super("RATIONAL");

				final BigInteger realLower = lowerInclusive ? lowerBound : lowerBound.add(BigInteger.ONE);
				final BigInteger realUpper = upperInclusive ? upperBound : upperBound.subtract(BigInteger.ONE);
				intervalDesignator = new IntervalDesignator<>(realLower, realUpper);
			}

			/**
			 * Gets instance of compound {@link RationalType} type.
			 *
			 * @param lowerBound
			 * 		the lower bound that this {@link RationalType} type includes
			 * @param lowerInclusive
			 * 		whether to include the lower bound in the interval
			 * @param upperBound
			 * 		the upper bound that this {@link RationalType} type includes
			 * @param upperInclusive
			 * 		whether to include the upper bound in the interval
			 *
			 * @return the newly created compound {@link RationalType} type
			 */
			public static RationalType getInstance(final BigInteger lowerBound, final boolean lowerInclusive,
			                                       final BigInteger upperBound, final boolean upperInclusive) {
				return new RationalTypeImpl(lowerBound, lowerInclusive, upperBound, upperInclusive);
			}

			@Override
			public boolean typeEquals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof RationalType)) {
					return false;
				}

				final RationalType rationalType = (RationalType) obj;
				if (rationalType == INSTANCE) {
					return true;
				}

				if (rationalType instanceof RationalTypeImpl) {
					final RationalTypeImpl rationalTypeImpl = (RationalTypeImpl) rationalType;

					return (intervalDesignator == null) || intervalDesignator.equals(rationalTypeImpl.intervalDesignator);
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
