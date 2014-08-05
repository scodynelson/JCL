package jcl.types;

import jcl.structs.packages.GlobalPackageStruct;
import jcl.typespecifiers.AtomicTypeSpecifier;
import jcl.typespecifiers.CompoundTypeSpecifier;
import jcl.typespecifiers.designator.IntervalDesignator;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.String;
import java.math.BigDecimal;

/**
 * A {@link LongFloat} is a {@link Float} type with a minimum of 50 precision bits and 8 exponent bits.
 * <p>
 * {@link LongFloat} -> {@link Float} -> {@link Real} -> {@link Number} -> {@link T}
 */
public interface LongFloat extends Float {

	LongFloat INSTANCE = new Factory.LongFloatImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<LongFloat> {

		@Override
		public LongFloat getInstance() {
			return INSTANCE;
		}

		/**
		 * Gets instance of compound {@link LongFloat} type.
		 *
		 * @param lowerBound the lower bound that this {@link LongFloat} type includes
		 * @param upperBound the upper bound that this {@link LongFloat} type includes
		 * @return the newly created compound {@link LongFloat} type
		 */
		public static LongFloat getInstance(final BigDecimal lowerBound, final BigDecimal upperBound) {
			return LongFloatImpl.getInstance(lowerBound, true, upperBound, true);
		}

		/**
		 * Gets instance of compound {@link LongFloat} type.
		 *
		 * @param lowerBound     the lower bound that this {@link LongFloat} type includes
		 * @param lowerInclusive whether to include the lower bound in the interval
		 * @param upperBound     the upper bound that this {@link LongFloat} type includes
		 * @param upperInclusive whether to include the upper bound in the interval
		 * @return the newly created compound {@link LongFloat} type
		 */
		public static LongFloat getInstance(final BigDecimal lowerBound, final boolean lowerInclusive,
		                                    final BigDecimal upperBound, final boolean upperInclusive) {
			return LongFloatImpl.getInstance(lowerBound, lowerInclusive, upperBound, upperInclusive);
		}

		/**
		 * Inner {@link LongFloat} type implementation.
		 */
		private static class LongFloatImpl extends TypeBaseClass implements LongFloat, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private final IntervalDesignator<BigDecimal> intervalDesignator;

			/**
			 * Private constructor.
			 */
			private LongFloatImpl() {
				super("LONG-FLOAT", GlobalPackageStruct.COMMON_LISP);
				intervalDesignator = null;
			}

			/**
			 * Private constructor for compound {@link LongFloat} type.
			 *
			 * @param lowerBound     the lower bound that this {@link LongFloat} type includes
			 * @param lowerInclusive whether to include the lower bound in the interval
			 * @param upperBound     the upper bound that this {@link LongFloat} type includes
			 * @param upperInclusive whether to include the upper bound in the interval
			 */
			private LongFloatImpl(final BigDecimal lowerBound, final boolean lowerInclusive,
			                      final BigDecimal upperBound, final boolean upperInclusive) {
				super("LONG-FLOAT", GlobalPackageStruct.COMMON_LISP);

				final BigDecimal realLower = lowerInclusive ? lowerBound : lowerBound.add(BigDecimal.ONE);
				final BigDecimal realUpper = upperInclusive ? upperBound : upperBound.subtract(BigDecimal.ONE);
				intervalDesignator = new IntervalDesignator<>(realLower, realUpper);
			}

			/**
			 * Gets instance of compound {@link LongFloat} type.
			 *
			 * @param lowerBound     the lower bound that this {@link LongFloat} type includes
			 * @param lowerInclusive whether to include the lower bound in the interval
			 * @param upperBound     the upper bound that this {@link LongFloat} type includes
			 * @param upperInclusive whether to include the upper bound in the interval
			 * @return the newly created compound {@link LongFloat} type
			 */
			public static LongFloat getInstance(final BigDecimal lowerBound, final boolean lowerInclusive,
			                                    final BigDecimal upperBound, final boolean upperInclusive) {
				return new LongFloatImpl(lowerBound, lowerInclusive, upperBound, upperInclusive);
			}

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof LongFloat)) {
					return false;
				}

				final LongFloat longFloat = (LongFloat) obj;
				if (longFloat == INSTANCE) {
					return true;
				}

				if (longFloat instanceof LongFloatImpl) {
					final LongFloatImpl longFloatImpl = (LongFloatImpl) longFloat;

					return (intervalDesignator == null) || intervalDesignator.equals(longFloatImpl.intervalDesignator);
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
				return "LongFloatImpl{"
						+ "intervalDesignator=" + intervalDesignator
						+ '}';
			}
		}
	}
}
