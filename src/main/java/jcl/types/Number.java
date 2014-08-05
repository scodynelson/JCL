package jcl.types;

import jcl.structs.packages.GlobalPackageStruct;
import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.String;

/**
 * A {@link Number} contains objects which represent mathematical numbers. The types {@link Real} and {@link Complex}
 * are disjoint subtypes of {@link Number}.
 * <p>
 * Notes:
 * Common Lisp differs from mathematics on some naming issues. In mathematics, the set of real numbers is traditionally
 * described as a subset of the complex numbers, but in Common Lisp, the type {@link Real} and the type {@link Complex}
 * are disjoint. The Common Lisp type which includes all mathematical complex numbers is called number. The reasons for
 * these differences include historical precedent, compatibility with most other popular computer languages, and various
 * issues of time and space efficiency.
 * <p>
 * {@link Number} -> {@link T}
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
		 * Inner {@link Number} type implementation.
		 */
		private static class NumberImpl extends TypeBaseClass implements Number, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private NumberImpl() {
				super("NUMBER", GlobalPackageStruct.COMMON_LISP);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof Number);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}

			@Override
			public String toString() {
				return "NumberImpl{}";
			}
		}
	}
}
