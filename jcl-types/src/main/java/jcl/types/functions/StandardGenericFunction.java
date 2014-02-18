package jcl.types.functions;

import jcl.types.TypeFactory;
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
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof StandardGenericFunction)) {
					return false;
				}

				final StandardGenericFunction standardGenericFunction = (StandardGenericFunction) obj;
				return standardGenericFunction == INSTANCE;
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}
