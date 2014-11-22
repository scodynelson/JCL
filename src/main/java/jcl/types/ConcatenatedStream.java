package jcl.types;

import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;

/**
 * A {@link ConcatenatedStream} is an input stream which is a composite stream of zero or more other input streams,
 * such that the sequence of data which can be read from the {@link ConcatenatedStream} is the same as the
 * concatenation of the sequences of data which could be read from each of the constituent streams.
 * <p>
 * Input from a {@link ConcatenatedStream} is taken from the first of the associated input streams until it reaches end
 * of file; then that stream is discarded, and subsequent input is taken from the next input stream, and so on. An end
 * of file on the associated input streams is always managed invisibly by the {@link ConcatenatedStream}---the only
 * time a client of a {@link ConcatenatedStream} sees an end of file is when an attempt is made to obtain data from the
 * {@link ConcatenatedStream} but it has no remaining input streams from which to obtain such data.
 * <p>
 * {@link ConcatenatedStream} -> {@link Stream} -> {@link T}
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
		 * Inner {@link ConcatenatedStream} type implementation.
		 */
		private static final class ConcatenatedStreamImpl extends TypeBaseClass implements ConcatenatedStream, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private ConcatenatedStreamImpl() {
				super("CONCATENATED-STREAM");
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof ConcatenatedStream);
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}
