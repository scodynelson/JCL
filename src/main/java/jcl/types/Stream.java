package jcl.types;

import jcl.structs.packages.GlobalPackageStruct;
import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;

/**
 * A {@link Stream} is an object that can be used with an input or output function to identify an appropriate source or
 * sink of characters or bytes for that operation.
 * <p>
 * {@link Stream} -> {@link T}
 */
public interface Stream extends T {

	Stream INSTANCE = new Factory.StreamImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Stream> {

		@Override
		public Stream getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Stream} type implementation.
		 */
		private static class StreamImpl extends TypeBaseClass implements Stream, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private StreamImpl() {
				super("STREAM", GlobalPackageStruct.COMMON_LISP);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof Stream);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}
