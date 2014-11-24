package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;

/**
 * An {@link EchoStream} is a bidirectional composite stream that receives its input from an associated input stream
 * and sends its output to an associated output stream. All input taken from the input stream is echoed to the output
 * stream.
 * <p>
 * {@link EchoStream} -> {@link Stream} -> {@link T}
 */
public interface EchoStream extends Stream {

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
		 * Inner {@link EchoStream} type implementation.
		 */
		private static final class EchoStreamImpl extends TypeBaseClass implements EchoStream, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private EchoStreamImpl() {
				super("ECHO-STREAM");
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof EchoStream);
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}
