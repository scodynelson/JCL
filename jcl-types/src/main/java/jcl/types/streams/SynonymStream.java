package jcl.types.streams;

import jcl.types.T;
import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code SynonymStream} stream is a stream that is an alias for another stream, which is the value of a dynamic variable
 * whose name is the synonym stream symbol of the {@code SynonymStream}.
 * <p/>
 * Any operations on a {@code SynonymStream} will be performed on the stream that is then the value of the dynamic variable
 * named by the synonym stream symbol. If the value of the variable should change, or if the variable should be bound,
 * then the stream will operate on the new value of the variable.
 */
public interface SynonymStream extends Stream, T {

	SynonymStream INSTANCE = new Factory.SynonymStreamImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<SynonymStream> {

		@Override
		public SynonymStream getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@code SynonymStream} type implementation.
		 */
		private static class SynonymStreamImpl implements SynonymStream, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof SynonymStream)) {
					return false;
				}

				final SynonymStream synonymStream = (SynonymStream) obj;
				return synonymStream == INSTANCE;
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}
