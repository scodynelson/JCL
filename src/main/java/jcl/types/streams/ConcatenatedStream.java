package jcl.types.streams;

import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code ConcatenatedStream} is an input stream which is a composite stream of zero or more other input streams, such
 * that the sequence of data which can be read from the {@code ConcatenatedStream} is the same as the concatenation of
 * the sequences of data which could be read from each of the constituent streams.
 * <p/>
 * Input from a {@code ConcatenatedStream} is taken from the first of the associated input streams until it reaches end
 * of file; then that stream is discarded, and subsequent input is taken from the next input stream, and so on. An end
 * of file on the associated input streams is always managed invisibly by the {@code ConcatenatedStream}---the only time
 * a client of a {@code ConcatenatedStream} sees an end of file is when an attempt is made to obtain data from the
 * {@code ConcatenatedStream} but it has no remaining input streams from which to obtain such data.
 * <p/>
 * {@code ConcatenatedStream} -> {@code Stream} -> {@code T}
 */
public interface ConcatenatedStream extends Stream {

	ConcatenatedStream INSTANCE = new Factory.ConcatenatedStreamImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<ConcatenatedStream> {

		@Override
		public ConcatenatedStream getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@code ConcatenatedStream} type implementation.
		 */
		private static class ConcatenatedStreamImpl implements ConcatenatedStream, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof ConcatenatedStream);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}
