package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;

/**
 * A {@link BroadcastStream} is an output stream which which is a composite stream of zero or more other output
 * streams, such that any output sent to the {@link BroadcastStream} gets passed on as output to each of the associated
 * output streams.
 * <p>
 * If a {@link BroadcastStream} has no component streams, then all output to the {@link BroadcastStream} is discarded.
 * <p>
 * The set of operations that may be performed on a broadcast stream is the intersection of those for its associated
 * output streams.
 * <p>
 * {@link BroadcastStream} -> {@link Stream} -> {@link T}
 */
public interface BroadcastStream extends Stream {

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
		 * Inner {@link BroadcastStream} type implementation.
		 */
		private static final class BroadcastStreamImpl extends TypeBaseClass implements BroadcastStream, AtomicTypeSpecifier {

			private static final long serialVersionUID = 4069710741454263582L;

			/**
			 * Private constructor.
			 */
			private BroadcastStreamImpl() {
				super("BROADCAST-STREAM");
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof BroadcastStream);
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}
