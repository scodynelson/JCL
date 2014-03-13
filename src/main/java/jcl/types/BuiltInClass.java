package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code BuiltInClass} is a {@code Class} whose instances have restricted capabilities or special representations.
 * {@code BuiltInClass}es can be used as parameter specializers in {@code Methods}.
 * <p/>
 * {@code BuiltInClass} -> {@code Class} -> {@code StandardObject} -> {@code T}
 */
public interface BuiltInClass extends Class {

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
				return (this == obj) || (obj instanceof BuiltInClass);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}
