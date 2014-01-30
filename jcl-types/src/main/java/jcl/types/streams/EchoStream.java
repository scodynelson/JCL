package jcl.types.streams;

import jcl.types.T;
import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * An {@code EchoStream} is a bidirectional composite stream that receives its input from an associated input stream and
 * sends its output to an associated output stream. All input taken from the input stream is echoed to the output stream.
 */
public interface EchoStream extends Stream, T {

	EchoStream INSTANCE = new Factory.EchoStreamImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<EchoStream> {

		@Override
		public EchoStream getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@code EchoStream} type implementation.
		 */
		private static class EchoStreamImpl implements EchoStream, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof EchoStream)) {
					return false;
				}

				final EchoStream echoStream = (EchoStream) obj;
				return echoStream == INSTANCE;
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}
