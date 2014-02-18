package jcl.types.numbers;

import jcl.types.T;
import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code Number} contains objects which represent mathematical numbers. The types {@code Real} and {@code Complex}
 * are disjoint subtypes of {@code Number}.
 * <p/>
 * Notes:
 * Common Lisp differs from mathematics on some naming issues. In mathematics, the set of real numbers is traditionally
 * described as a subset of the complex numbers, but in Common Lisp, the type {@code Real} and the type {@code Complex}
 * are disjoint. The Common Lisp type which includes all mathematical complex numbers is called number. The reasons for
 * these differences include historical precedent, compatibility with most other popular computer languages, and various
 * issues of time and space efficiency.
 * <p/>
 * {@code Number} -> {@code T}
 */
public interface Number extends T {

	Number INSTANCE = new Factory.NumberImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Number> {

		@Override
		public Number getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@code Number} type implementation.
		 */
		private static class NumberImpl implements Number, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof Number)) {
					return false;
				}

				final Number number = (Number) obj;
				return number == INSTANCE;
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}
