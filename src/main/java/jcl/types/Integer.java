/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import jcl.types.typespecifiers.CompoundTypeSpecifier;
import jcl.types.typespecifiers.designator.IntervalDesignator;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.String;
import java.math.BigInteger;

/**
 * An {@link Integer} is a mathematical integer. There is no limit on the magnitude of an {@link Integer}.
 * <p>
 * The types {@link Fixnum} and {@link Bignum} form an exhaustive partition of type {@link Integer}.
 * <p>
 * {@link Integer} -> {@link Rational} -> {@link Real} -> {@link Number} -> {@link T}
 */
public interface Integer extends Rational {

	/**
	 * Singleton instance of the {@link Integer} type.
	 */
	Integer INSTANCE = new Factory.IntegerImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Integer> {

		/**
		 * Gets instance of compound {@link Integer} type.
		 *
		 * @param lowerBound
		 * 		the lower bound that this {@link Integer} type includes
		 * @param upperBound
		 * 		the upper bound that this {@link Integer} type includes
		 *
		 * @return the newly created compound {@link Integer} type
		 */
		public static Integer getInstance(final BigInteger lowerBound, final BigInteger upperBound) {
			return IntegerImpl.getInstance(lowerBound, true, upperBound, true);
		}

		/**
		 * Gets instance of compound {@link Integer} type.
		 *
		 * @param lowerBound
		 * 		the lower bound that this {@link Integer} type includes
		 * @param lowerInclusive
		 * 		whether to include the lower bound in the interval
		 * @param upperBound
		 * 		the upper bound that this {@link Integer} type includes
		 * @param upperInclusive
		 * 		whether to include the upper bound in the interval
		 *
		 * @return the newly created compound {@link Integer} type
		 */
		public static Integer getInstance(final BigInteger lowerBound, final boolean lowerInclusive,
		                                  final BigInteger upperBound, final boolean upperInclusive) {
			return IntegerImpl.getInstance(lowerBound, lowerInclusive, upperBound, upperInclusive);
		}

		@Override
		public Integer getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Integer} type implementation.
		 */
		private static final class IntegerImpl extends TypeBaseClass implements Integer, AtomicTypeSpecifier, CompoundTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 7322232418971038179L;

			/**
			 * The interval range in which the {@link Integer} type exists.
			 */
			private final IntervalDesignator<BigInteger> intervalDesignator;

			/**
			 * Private constructor.
			 */
			private IntegerImpl() {
				super("INTEGER");
				intervalDesignator = null;
			}

			/**
			 * Private constructor for compound {@link Integer} type.
			 *
			 * @param lowerBound
			 * 		the lower bound that this {@link Integer} type includes
			 * @param lowerInclusive
			 * 		whether to include the lower bound in the interval
			 * @param upperBound
			 * 		the upper bound that this {@link Integer} type includes
			 * @param upperInclusive
			 * 		whether to include the upper bound in the interval
			 */
			private IntegerImpl(final BigInteger lowerBound, final boolean lowerInclusive,
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
			 * Gets instance of compound {@link Integer} type.
			 *
			 * @param lowerBound
			 * 		the lower bound that this {@link Integer} type includes
			 * @param lowerInclusive
			 * 		whether to include the lower bound in the interval
			 * @param upperBound
			 * 		the upper bound that this {@link Integer} type includes
			 * @param upperInclusive
			 * 		whether to include the upper bound in the interval
			 *
			 * @return the newly created compound {@link Integer} type
			 */
			public static Integer getInstance(final BigInteger lowerBound, final boolean lowerInclusive,
			                                  final BigInteger upperBound, final boolean upperInclusive) {
				return new IntegerImpl(lowerBound, lowerInclusive, upperBound, upperInclusive);
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

				if (!(obj instanceof Integer)) {
					return false;
				}

				final Integer integer = (Integer) obj;
				if (integer == INSTANCE) {
					return true;
				}

				if (integer instanceof IntegerImpl) {
					final IntegerImpl integerImpl = (IntegerImpl) integer;

					return (intervalDesignator == null) || intervalDesignator.equals(integerImpl.intervalDesignator);
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
