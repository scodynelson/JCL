package jcl.types.streams;

import jcl.types.T;
import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code BroadcastStream} is an output stream which which is a composite stream of zero or more other output streams,such
 * that any output sent to the {@code BroadcastStream} gets passed on as output to each of the associated output streams.
 * <p/>
 * If a {@code BroadcastStream} has no component streams, then all output to the {@code BroadcastStream} is discarded.
 * <p/>
 * The set of operations that may be performed on a broadcast stream is the intersection of those for its associated output
 * streams.
 */
public interface BroadcastStream extends Stream, T {

	BroadcastStream INSTANCE = new Factory.BroadcastStreamImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<BroadcastStream> {

		@Override
		public BroadcastStream getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@code BroadcastStream} type implementation.
		 */
		private static class BroadcastStreamImpl implements BroadcastStream, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof BroadcastStream)) {
					return false;
				}

				final BroadcastStream broadcastStream = (BroadcastStream) obj;
				return broadcastStream == INSTANCE;
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}
