package jcl.types.numbers;

import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code Ratio} is a {@code Number} representing the mathematical ratio of two non-zero integers, the numerator and
 * denominator, whose greatest common divisor is one, and of which the denominator is positive and greater than one.
 * <p/>
 * {@code Ratio} -> {@code Rational} -> {@code Real} -> {@code Number} -> {@code T}
 */
public interface Ratio extends Rational {

	Ratio INSTANCE = new Factory.RatioImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Ratio> {

		@Override
		public Ratio getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@code Ratio} type implementation.
		 */
		private static class RatioImpl implements Ratio, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof Ratio);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}
