package jcl.types.classes;

import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code StandardClass} is the default class type {@code Class}.
 * <p/>
 * {@code StandardClass} -> {@code Class} -> {@code StandardObject} -> {@code T}
 */
public interface StandardClass extends Class {

	StandardClass INSTANCE = new Factory.StandardClassImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<StandardClass> {

		@Override
		public StandardClass getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@code StandardClass} type implementation.
		 */
		private static class StandardClassImpl implements StandardClass, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof StandardClass)) {
					return false;
				}

				final StandardClass standardClass = (StandardClass) obj;
				return standardClass == INSTANCE;
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}
