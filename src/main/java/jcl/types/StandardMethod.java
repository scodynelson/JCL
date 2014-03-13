package jcl.types;

import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code StandardMethod} is the default method type {@code Class}.
 * <p/>
 * {@code StandardMethod} -> {@code Method} -> {@code StandardObject} -> {@code T}
 */
public interface StandardMethod extends Method, StandardObject {

	StandardMethod INSTANCE = new Factory.StandardMethodImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<StandardMethod> {

		@Override
		public StandardMethod getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@code StandardMethod} type implementation.
		 */
		private static class StandardMethodImpl implements StandardMethod, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof StandardMethod);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}
