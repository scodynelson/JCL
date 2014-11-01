package jcl.types;

import jcl.structs.packages.GlobalPackageStruct;
import jcl.typespecifiers.AtomicTypeSpecifier;
import jcl.typespecifiers.CompoundTypeSpecifier;
import jcl.typespecifiers.designator.IntervalDesignator;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

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

	Integer INSTANCE = new Factory.IntegerImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Integer> {

		@Override
		public Integer getInstance() {
			return INSTANCE;
		}

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

		/**
		 * Inner {@link Integer} type implementation.
		 */
		private static class IntegerImpl extends TypeBaseClass implements Integer, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private final IntervalDesignator<BigInteger> intervalDesignator;

			/**
			 * Private constructor.
			 */
			private IntegerImpl() {
				super("INTEGER", GlobalPackageStruct.COMMON_LISP);
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
				super("INTEGER", GlobalPackageStruct.COMMON_LISP);

				BigInteger realLower = null;
				if (lowerBound != null) {
					realLower = lowerInclusive ? lowerBound : lowerBound.add(BigInteger.ONE);
				}

				BigInteger realUpper = null;
				if (lowerBound != null) {
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
			public int hashCode() {
				return new HashCodeBuilder()
						.append(intervalDesignator)
						.toHashCode();
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}
