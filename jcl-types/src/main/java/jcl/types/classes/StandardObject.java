package jcl.types.classes;

import jcl.types.T;
import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * The {@code StandardObject} is an instance of {@code StandardClass} and is a superclass of every {@code Class} that
 * is an instance of {@code StandardClass} except itself.
 * <p/>
 * {@code StandardObject} -> {@code T}
 */
public interface StandardObject extends T {

	StandardObject INSTANCE = new Factory.StandardObjectImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<StandardObject> {

		@Override
		public StandardObject getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@code StandardObject} type implementation.
		 */
		private static class StandardObjectImpl implements StandardObject, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof StandardObject);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}
