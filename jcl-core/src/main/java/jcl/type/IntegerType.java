/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import java.math.BigInteger;

import jcl.type.typespecifier.AtomicTypeSpecifier;
import jcl.type.typespecifier.CompoundTypeSpecifier;

/**
 * An {@link IntegerType} is a mathematical integer. There is no limit on the magnitude of an {@link IntegerType}.
 * <p>
 * The types {@link FixnumType} and {@link BignumType} form an exhaustive partition of type {@link IntegerType}.
 * <p>
 * {@link IntegerType} -&gt; {@link RationalType} -&gt; {@link RealType} -&gt; {@link NumberType} -&gt; {@link TType}
 */
public interface IntegerType extends RationalType {

	/**
	 * Singleton instance of the {@link IntegerType} type.
	 */
	IntegerType INSTANCE = new Factory.IntegerTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<IntegerType> {

		/**
		 * Gets instance of compound {@link IntegerType} type.
		 *
		 * @param lowerBound
		 * 		the lower bound that this {@link IntegerType} type includes
		 * @param upperBound
		 * 		the upper bound that this {@link IntegerType} type includes
		 *
		 * @return the newly created compound {@link IntegerType} type
		 */
		public static IntegerType getInstance(final BigInteger lowerBound, final BigInteger upperBound) {
			return IntegerTypeImpl.getInstance(lowerBound, true, upperBound, true);
		}

		/**
		 * Gets instance of compound {@link IntegerType} type.
		 *
		 * @param lowerBound
		 * 		the lower bound that this {@link IntegerType} type includes
		 * @param lowerInclusive
		 * 		whether to include the lower bound in the interval
		 * @param upperBound
		 * 		the upper bound that this {@link IntegerType} type includes
		 * @param upperInclusive
		 * 		whether to include the upper bound in the interval
		 *
		 * @return the newly created compound {@link IntegerType} type
		 */
		public static IntegerType getInstance(final BigInteger lowerBound, final boolean lowerInclusive,
		                                      final BigInteger upperBound, final boolean upperInclusive) {
			return IntegerTypeImpl.getInstance(lowerBound, lowerInclusive, upperBound, upperInclusive);
		}

		@Override
		public IntegerType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link IntegerType} type implementation.
		 */
		private static final class IntegerTypeImpl extends TypeBaseClass implements IntegerType, AtomicTypeSpecifier, CompoundTypeSpecifier {

			/**
			 * The interval range in which the {@link IntegerType} type exists.
			 */
			private final IntervalDesignator<BigInteger> intervalDesignator;

			/**
			 * Private constructor.
			 */
			private IntegerTypeImpl() {
				super("INTEGER");
				intervalDesignator = null;
			}

			/**
			 * Private constructor for compound {@link IntegerType} type.
			 *
			 * @param lowerBound
			 * 		the lower bound that this {@link IntegerType} type includes
			 * @param lowerInclusive
			 * 		whether to include the lower bound in the interval
			 * @param upperBound
			 * 		the upper bound that this {@link IntegerType} type includes
			 * @param upperInclusive
			 * 		whether to include the upper bound in the interval
			 */
			private IntegerTypeImpl(final BigInteger lowerBound, final boolean lowerInclusive,
			                        final BigInteger upperBound, final boolean upperInclusive) {
				super("INTEGER");

				BigInteger realLower = null;
				if (lowerBound != null) {
					realLower = lowerInclusive ? lowerBound : lowerBound.add(BigInteger.ONE);
				}

				BigInteger realUpper = null;
				if (upperBound != null) {
					realUpper = upperInclusive ? upperBound : upperBound.subtract(BigInteger.ONE);
				}

				intervalDesignator = new IntervalDesignator<>(realLower, realUpper);
			}

			/**
			 * Gets instance of compound {@link IntegerType} type.
			 *
			 * @param lowerBound
			 * 		the lower bound that this {@link IntegerType} type includes
			 * @param lowerInclusive
			 * 		whether to include the lower bound in the interval
			 * @param upperBound
			 * 		the upper bound that this {@link IntegerType} type includes
			 * @param upperInclusive
			 * 		whether to include the upper bound in the interval
			 *
			 * @return the newly created compound {@link IntegerType} type
			 */
			public static IntegerType getInstance(final BigInteger lowerBound, final boolean lowerInclusive,
			                                      final BigInteger upperBound, final boolean upperInclusive) {
				return new IntegerTypeImpl(lowerBound, lowerInclusive, upperBound, upperInclusive);
			}

			@Override
			public boolean typeEquals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof IntegerType)) {
					return false;
				}

				final IntegerType integerType = (IntegerType) obj;
				if (integerType == INSTANCE) {
					return true;
				}

				if (integerType instanceof IntegerTypeImpl) {
					final IntegerTypeImpl integerTypeImpl = (IntegerTypeImpl) integerType;

					return (intervalDesignator == null) || intervalDesignator.equals(integerTypeImpl.intervalDesignator);
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
