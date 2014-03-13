package jcl.types;

import jcl.typespecifiers.AtomicTypeSpecifier;
import jcl.typespecifiers.CompoundTypeSpecifier;
import jcl.typespecifiers.designator.IntervalDesignator;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.String;
import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * A {@code Real} includes all numbers that represent mathematical real numbers, though there are mathematical real numbers
 * (e.g., irrational numbers) that do not have an exact representation in Common Lisp.
 * <p/>
 * The types {@code Rational} and {@code Float} are disjoint subtypes of type {@code Real}.
 * <p/>
 * {@code Real} -> {@code Number} -> {@code T}
 */
public interface Real extends Number {

	Real INSTANCE = new Factory.RealImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Real> {

		@Override
		public Real getInstance() {
			return INSTANCE;
		}

		/**
		 * Gets instance of compound {@code Real} type.
		 *
		 * @param lowerBound the lower bound that this {@code Real} type includes
		 * @param upperBound the upper bound that this {@code Real} type includes
		 * @return the newly created compound {@code Real} type
		 */
		public static Real getInstance(final BigInteger lowerBound, final BigInteger upperBound) {
			return RealImpl.getInstance(lowerBound, true, upperBound, true);
		}

		/**
		 * Gets instance of compound {@code Real} type.
		 *
		 * @param lowerBound     the lower bound that this {@code Real} type includes
		 * @param lowerInclusive whether to include the lower bound in the interval
		 * @param upperBound     the upper bound that this {@code Real} type includes
		 * @param upperInclusive whether to include the upper bound in the interval
		 * @return the newly created compound {@code Real} type
		 */
		public static Real getInstance(final BigInteger lowerBound, final boolean lowerInclusive,
									   final BigInteger upperBound, final boolean upperInclusive) {
			return RealImpl.getInstance(lowerBound, lowerInclusive, upperBound, upperInclusive);
		}

		/**
		 * Gets instance of compound {@code Real} type.
		 *
		 * @param lowerBound the lower bound that this {@code Real} type includes
		 * @param upperBound the upper bound that this {@code Real} type includes
		 * @return the newly created compound {@code Real} type
		 */
		public static Real getInstance(final BigDecimal lowerBound, final BigDecimal upperBound) {
			return RealImpl.getInstance(lowerBound, true, upperBound, true);
		}

		/**
		 * Gets instance of compound {@code Real} type.
		 *
		 * @param lowerBound     the lower bound that this {@code Real} type includes
		 * @param lowerInclusive whether to include the lower bound in the interval
		 * @param upperBound     the upper bound that this {@code Real} type includes
		 * @param upperInclusive whether to include the upper bound in the interval
		 * @return the newly created compound {@code Real} type
		 */
		public static Real getInstance(final BigDecimal lowerBound, final boolean lowerInclusive,
									   final BigDecimal upperBound, final boolean upperInclusive) {
			return RealImpl.getInstance(lowerBound, lowerInclusive, upperBound, upperInclusive);
		}

		/**
		 * Inner {@code Real} type implementation.
		 */
		private static class RealImpl implements Real, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private final IntervalDesignator<BigInteger> integerIntervalDesignator;
			private final IntervalDesignator<BigDecimal> decimalIntervalDesignator;

			/**
			 * Private constructor.
			 */
			private RealImpl() {
				integerIntervalDesignator = null;
				decimalIntervalDesignator = null;
			}

			/**
			 * Private constructor for compound {@code Real} type.
			 *
			 * @param lowerBound     the lower bound that this {@code Real} type includes
			 * @param lowerInclusive whether to include the lower bound in the interval
			 * @param upperBound     the upper bound that this {@code Real} type includes
			 * @param upperInclusive whether to include the upper bound in the interval
			 */
			private RealImpl(final BigInteger lowerBound, final boolean lowerInclusive,
							 final BigInteger upperBound, final boolean upperInclusive) {

				final BigInteger realLower = lowerInclusive ? lowerBound : lowerBound.add(BigInteger.ONE);
				final BigInteger realUpper = upperInclusive ? upperBound : upperBound.subtract(BigInteger.ONE);

				integerIntervalDesignator = new IntervalDesignator<>(realLower, realUpper);
				decimalIntervalDesignator = null;
			}

			/**
			 * Private constructor for compound {@code Real} type.
			 *
			 * @param lowerBound     the lower bound that this {@code Real} type includes
			 * @param lowerInclusive whether to include the lower bound in the interval
			 * @param upperBound     the upper bound that this {@code Real} type includes
			 * @param upperInclusive whether to include the upper bound in the interval
			 */
			private RealImpl(final BigDecimal lowerBound, final boolean lowerInclusive,
							 final BigDecimal upperBound, final boolean upperInclusive) {

				final BigDecimal realLower = lowerInclusive ? lowerBound : lowerBound.add(BigDecimal.ONE);
				final BigDecimal realUpper = upperInclusive ? upperBound : upperBound.subtract(BigDecimal.ONE);

				decimalIntervalDesignator = new IntervalDesignator<>(realLower, realUpper);
				integerIntervalDesignator = null;
			}

			/**
			 * Gets instance of compound {@code Real} type.
			 *
			 * @param lowerBound     the lower bound that this {@code Real} type includes
			 * @param lowerInclusive whether to include the lower bound in the interval
			 * @param upperBound     the upper bound that this {@code Real} type includes
			 * @param upperInclusive whether to include the upper bound in the interval
			 * @return the newly created compound {@code Real} type
			 */
			public static Real getInstance(final BigInteger lowerBound, final boolean lowerInclusive,
										   final BigInteger upperBound, final boolean upperInclusive) {
				return new RealImpl(lowerBound, lowerInclusive, upperBound, upperInclusive);
			}

			/**
			 * Gets instance of compound {@code Real} type.
			 *
			 * @param lowerBound     the lower bound that this {@code Real} type includes
			 * @param lowerInclusive whether to include the lower bound in the interval
			 * @param upperBound     the upper bound that this {@code Real} type includes
			 * @param upperInclusive whether to include the upper bound in the interval
			 * @return the newly created compound {@code Real} type
			 */
			public static Real getInstance(final BigDecimal lowerBound, final boolean lowerInclusive,
										   final BigDecimal upperBound, final boolean upperInclusive) {
				return new RealImpl(lowerBound, lowerInclusive, upperBound, upperInclusive);
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
				if (real == INSTANCE) {
					return true;
				}

				if (real instanceof RealImpl) {
					final RealImpl realImpl = (RealImpl) real;

					if (integerIntervalDesignator != null) {
						return integerIntervalDesignator.equals(realImpl.integerIntervalDesignator);
					}

					return (decimalIntervalDesignator == null) || decimalIntervalDesignator.equals(realImpl.decimalIntervalDesignator);
				}

				return false;
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder()
						.append(integerIntervalDesignator)
						.append(decimalIntervalDesignator)
						.toHashCode();
			}

			@Override
			public String toString() {
				return "RealImpl{"
						+ "integerIntervalDesignator=" + integerIntervalDesignator
						+ ", decimalIntervalDesignator=" + decimalIntervalDesignator
						+ '}';
			}
		}
	}
}
