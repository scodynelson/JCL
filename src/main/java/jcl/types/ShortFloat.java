package jcl.types;

import jcl.structs.packages.GlobalPackageStruct;
import jcl.typespecifiers.AtomicTypeSpecifier;
import jcl.typespecifiers.CompoundTypeSpecifier;
import jcl.typespecifiers.designator.IntervalDesignator;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;
import java.math.BigDecimal;

/**
 * A {@link ShortFloat} is a {@link Float} type with a minimum of 13 precision bits and 5 exponent bits.
 * <p>
 * {@link ShortFloat} -> {@link Float} -> {@link Real} -> {@link Number} -> {@link T}
 */
public interface ShortFloat extends Float {

	ShortFloat INSTANCE = new Factory.ShortFloatImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<ShortFloat> {

		@Override
		public ShortFloat getInstance() {
			return INSTANCE;
		}

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

		/**
		 * Inner {@link ShortFloat} type implementation.
		 */
		private static class ShortFloatImpl extends TypeBaseClass implements ShortFloat, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private final IntervalDesignator<BigDecimal> intervalDesignator;

			/**
			 * Private constructor.
			 */
			private ShortFloatImpl() {
				super("SHORT-FLOAT", GlobalPackageStruct.COMMON_LISP);
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
				super("SHORT-FLOAT", GlobalPackageStruct.COMMON_LISP);

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
