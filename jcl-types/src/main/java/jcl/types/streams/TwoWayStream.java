package jcl.types.streams;

import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code TwoWayStream} is a bidirectional composite stream that receives its input from an associated input stream and
 * sends its output to an associated output stream.
 * <p/>
 * {@code TwoWayStream} -> {@code Stream} -> {@code T}
 */
public interface TwoWayStream extends Stream {

	TwoWayStream INSTANCE = new Factory.TwoWayStreamImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<TwoWayStream> {

		@Override
		public TwoWayStream getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@code TwoWayStream} type implementation.
		 */
		private static class TwoWayStreamImpl implements TwoWayStream, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof TwoWayStream)) {
					return false;
				}

				final TwoWayStream twoWayStream = (TwoWayStream) obj;
				return twoWayStream == INSTANCE;
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}
