package jcl.types;

import jcl.structs.packages.GlobalPackageStruct;
import jcl.typespecifiers.AtomicTypeSpecifier;
import jcl.typespecifiers.CompoundTypeSpecifier;
import jcl.typespecifiers.designator.IntervalDesignator;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.String;
import java.math.BigDecimal;

/**
 * A {@link SingleFloat} is a {@link Float} type with a minimum of 24 precision bits and 8 exponent bits.
 * <p>
 * {@link SingleFloat} -> {@link Float} -> {@link Real} -> {@link Number} -> {@link T}
 */
public interface SingleFloat extends Float {

	SingleFloat INSTANCE = new Factory.SingleFloatImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<SingleFloat> {

		@Override
		public SingleFloat getInstance() {
			return INSTANCE;
		}

		/**
		 * Gets instance of compound {@link SingleFloat} type.
		 *
		 * @param lowerBound the lower bound that this {@link SingleFloat} type includes
		 * @param upperBound the upper bound that this {@link SingleFloat} type includes
		 * @return the newly created compound {@link SingleFloat} type
		 */
		public static SingleFloat getInstance(final BigDecimal lowerBound, final BigDecimal upperBound) {
			return SingleFloatImpl.getInstance(lowerBound, true, upperBound, true);
		}

		/**
		 * Gets instance of compound {@link SingleFloat} type.
		 *
		 * @param lowerBound     the lower bound that this {@link SingleFloat} type includes
		 * @param lowerInclusive whether to include the lower bound in the interval
		 * @param upperBound     the upper bound that this {@link SingleFloat} type includes
		 * @param upperInclusive whether to include the upper bound in the interval
		 * @return the newly created compound {@link SingleFloat} type
		 */
		public static SingleFloat getInstance(final BigDecimal lowerBound, final boolean lowerInclusive,
		                                      final BigDecimal upperBound, final boolean upperInclusive) {
			return SingleFloatImpl.getInstance(lowerBound, lowerInclusive, upperBound, upperInclusive);
		}

		/**
		 * Inner {@link SingleFloat} type implementation.
		 */
		private static class SingleFloatImpl extends TypeBaseClass implements SingleFloat, AtomicTypeSpecifier, CompoundTypeSpecifier {

			private final IntervalDesignator<BigDecimal> intervalDesignator;

			/**
			 * Private constructor.
			 */
			private SingleFloatImpl() {
				super("SINGLE-FLOAT", GlobalPackageStruct.COMMON_LISP);
				intervalDesignator = null;
			}

			/**
			 * Private constructor for compound {@link SingleFloat} type.
			 *
			 * @param lowerBound     the lower bound that this {@link SingleFloat} type includes
			 * @param lowerInclusive whether to include the lower bound in the interval
			 * @param upperBound     the upper bound that this {@link SingleFloat} type includes
			 * @param upperInclusive whether to include the upper bound in the interval
			 */
			private SingleFloatImpl(final BigDecimal lowerBound, final boolean lowerInclusive,
			                        final BigDecimal upperBound, final boolean upperInclusive) {
				super("SINGLE-FLOAT", GlobalPackageStruct.COMMON_LISP);

				final BigDecimal realLower = lowerInclusive ? lowerBound : lowerBound.add(BigDecimal.ONE);
				final BigDecimal realUpper = upperInclusive ? upperBound : upperBound.subtract(BigDecimal.ONE);
				intervalDesignator = new IntervalDesignator<>(realLower, realUpper);
			}

			/**
			 * Gets instance of compound {@link SingleFloat} type.
			 *
			 * @param lowerBound     the lower bound that this {@link SingleFloat} type includes
			 * @param lowerInclusive whether to include the lower bound in the interval
			 * @param upperBound     the upper bound that this {@link SingleFloat} type includes
			 * @param upperInclusive whether to include the upper bound in the interval
			 * @return the newly created compound {@link SingleFloat} type
			 */
			public static SingleFloat getInstance(final BigDecimal lowerBound, final boolean lowerInclusive,
			                                      final BigDecimal upperBound, final boolean upperInclusive) {
				return new SingleFloatImpl(lowerBound, lowerInclusive, upperBound, upperInclusive);
			}

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof SingleFloat)) {
					return false;
				}

				final SingleFloat singleFloat = (SingleFloat) obj;
				if (singleFloat == INSTANCE) {
					return true;
				}

				if (singleFloat instanceof SingleFloatImpl) {
					final SingleFloatImpl singleFloatImpl = (SingleFloatImpl) singleFloat;

					return (intervalDesignator == null) || intervalDesignator.equals(singleFloatImpl.intervalDesignator);
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
				return "SingleFloatImpl{"
						+ "intervalDesignator=" + intervalDesignator
						+ '}';
			}
		}
	}
}
