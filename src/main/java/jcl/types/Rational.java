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
 * A {@link Rational} is a {@link Number} with a canonical representation of an {@link Integer} if its value is
 * integral, and otherwise as a {@link Ratio}.
 * <p>
 * The types {@link Integer} and {@link Ratio} are disjoint subtypes of type {@link Rational}.
 * <p>
 * {@link Rational} -> {@link Real} -> {@link Number} -> {@link T}
 */
public interface Rational extends Real {

	Rational INSTANCE = new Factory.RationalImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Rational> {

		/**
		 * Gets instance of compound {@link Rational} type.
		 *
		 * @param lowerBound
		 * 		the lower bound that this {@link Rational} type includes
		 * @param upperBound
		 * 		the upper bound that this {@link Rational} type includes
		 *
		 * @return the newly created compound {@link Rational} type
		 */
		public static Rational getInstance(final BigInteger lowerBound, final BigInteger upperBound) {
			return RationalImpl.getInstance(lowerBound, true, upperBound, true);
		}

		/**
		 * Gets instance of compound {@link Rational} type.
		 *
		 * @param lowerBound
		 * 		the lower bound that this {@link Rational} type includes
		 * @param lowerInclusive
		 * 		whether to include the lower bound in the interval
		 * @param upperBound
		 * 		the upper bound that this {@link Rational} type includes
		 * @param upperInclusive
		 * 		whether to include the upper bound in the interval
		 *
		 * @return the newly created compound {@link Rational} type
		 */
		public static Rational getInstance(final BigInteger lowerBound, final boolean lowerInclusive,
		                                   final BigInteger upperBound, final boolean upperInclusive) {
			return RationalImpl.getInstance(lowerBound, lowerInclusive, upperBound, upperInclusive);
		}

		@Override
		public Rational getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Rational} type implementation.
		 */
		private static final class RationalImpl extends TypeBaseClass implements Rational, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private final IntervalDesignator<BigInteger> intervalDesignator;

			/**
			 * Private constructor.
			 */
			private RationalImpl() {
				super("RATIONAL", GlobalPackageStruct.COMMON_LISP);
				intervalDesignator = null;
			}

			/**
			 * Private constructor for compound {@link Rational} type.
			 *
			 * @param lowerBound
			 * 		the lower bound that this {@link Rational} type includes
			 * @param lowerInclusive
			 * 		whether to include the lower bound in the interval
			 * @param upperBound
			 * 		the upper bound that this {@link Rational} type includes
			 * @param upperInclusive
			 * 		whether to include the upper bound in the interval
			 */
			private RationalImpl(final BigInteger lowerBound, final boolean lowerInclusive,
			                     final BigInteger upperBound, final boolean upperInclusive) {
				super("RATIONAL", GlobalPackageStruct.COMMON_LISP);

				final BigInteger realLower = lowerInclusive ? lowerBound : lowerBound.add(BigInteger.ONE);
				final BigInteger realUpper = upperInclusive ? upperBound : upperBound.subtract(BigInteger.ONE);
				intervalDesignator = new IntervalDesignator<>(realLower, realUpper);
			}

			/**
			 * Gets instance of compound {@link Rational} type.
			 *
			 * @param lowerBound
			 * 		the lower bound that this {@link Rational} type includes
			 * @param lowerInclusive
			 * 		whether to include the lower bound in the interval
			 * @param upperBound
			 * 		the upper bound that this {@link Rational} type includes
			 * @param upperInclusive
			 * 		whether to include the upper bound in the interval
			 *
			 * @return the newly created compound {@link Rational} type
			 */
			public static Rational getInstance(final BigInteger lowerBound, final boolean lowerInclusive,
			                                   final BigInteger upperBound, final boolean upperInclusive) {
				return new RationalImpl(lowerBound, lowerInclusive, upperBound, upperInclusive);
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

				if (!(obj instanceof Rational)) {
					return false;
				}

				final Rational rational = (Rational) obj;
				if (rational == INSTANCE) {
					return true;
				}

				if (rational instanceof RationalImpl) {
					final RationalImpl rationalImpl = (RationalImpl) rational;

					return (intervalDesignator == null) || intervalDesignator.equals(rationalImpl.intervalDesignator);
				}

				return false;
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}
