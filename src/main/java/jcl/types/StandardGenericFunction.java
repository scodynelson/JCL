package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * The class {@code StandardGenericFunction} is the default {@code Class} of {@code GenericFunction}s.
 * <p/>
 * {@code StandardGenericFunction} -> {@code GenericFunction} -> {@code Function} -> {@code T}
 */
public interface StandardGenericFunction extends GenericFunction {

	StandardGenericFunction INSTANCE = new Factory.StandardGenericFunctionImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<StandardGenericFunction> {

		@Override
		public StandardGenericFunction getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@code StandardGenericFunction} type implementation.
		 */
		private static class StandardGenericFunctionImpl implements StandardGenericFunction, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof StandardGenericFunction);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}
