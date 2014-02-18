package jcl.types.streams;

import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code StringStream} is a stream which reads input from or writes output to an associated {@code String}.
 * <p/>
 * The stream element type of a {@code StringStream} is always a subtype of type {@code Character}.
 * <p/>
 * {@code StringStream} -> {@code Stream} -> {@code T}
 */
public interface StringStream extends Stream {

	StringStream INSTANCE = new Factory.StringStreamImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<StringStream> {

		@Override
		public StringStream getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@code StringStream} type implementation.
		 */
		private static class StringStreamImpl implements StringStream, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof StringStream)) {
					return false;
				}

				final StringStream stringStream = (StringStream) obj;
				return stringStream == INSTANCE;
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}
