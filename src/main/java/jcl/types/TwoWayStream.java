package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.String;

/**
 * A {@link TwoWayStream} is a bidirectional composite stream that receives its input from an associated input stream
 * and sends its output to an associated output stream.
 * <p>
 * {@link TwoWayStream} -> {@link Stream} -> {@link T}
 */
public interface TwoWayStream extends Stream {

	/**
	 * Singleton instance of the {@link TwoWayStream} type.
	 */
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
		 * Inner {@link TwoWayStream} type implementation.
		 */
		private static final class TwoWayStreamImpl extends TypeBaseClass implements TwoWayStream, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = -6594390772220907590L;

			/**
			 * Private constructor.
			 */
			private TwoWayStreamImpl() {
				super("TWO-WAY-STREAM");
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof TwoWayStream);
			}

			@Override
			public String toString() {
//				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
				return getName();
			}
		}
	}
}
