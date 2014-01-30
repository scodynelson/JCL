package jcl.types.streams;

import jcl.types.T;
import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code Stream} is an object that can be used with an input or output function to identify an appropriate source or
 * sink of characters or bytes for that operation.
 */
public interface Stream extends T {

	Stream INSTANCE = new Factory.StreamImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Stream> {

		@Override
		public Stream getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@code Stream} type implementation.
		 */
		private static class StreamImpl implements Stream, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof Stream)) {
					return false;
				}

				final Stream stream = (Stream) obj;
				return stream == INSTANCE;
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}
