package jcl.types;

import jcl.structs.packages.GlobalPackageStruct;
import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;

/**
 * A {@link StringStream} is a stream which reads input from or writes output to an associated {@link String}.
 * <p>
 * The stream element type of a {@link StringStream} is always a subtype of type {@link Character}.
 * <p>
 * {@link StringStream} -> {@link Stream} -> {@link T}
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
		 * Inner {@link StringStream} type implementation.
		 */
		private static class StringStreamImpl extends TypeBaseClass implements StringStream, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private StringStreamImpl() {
				super("STRING-STREAM", GlobalPackageStruct.COMMON_LISP);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof StringStream);
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}
