package jcl.types.classes;

import jcl.types.T;
import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code BuiltInClass} is a {@code Class} whose instances have restricted capabilities or special representations.
 * {@code BuiltInClass}es can be used as parameter specializers in {@code Methods}.
 */
public interface BuiltInClass extends Class, StandardObject, T {

	BuiltInClass INSTANCE = new Factory.BuiltInClassImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<BuiltInClass> {

		@Override
		public BuiltInClass getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@code BuiltInClass} type implementation.
		 */
		private static class BuiltInClassImpl implements BuiltInClass, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof BuiltInClass)) {
					return false;
				}

				final BuiltInClass builtInClass = (BuiltInClass) obj;
				return builtInClass == INSTANCE;
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}
