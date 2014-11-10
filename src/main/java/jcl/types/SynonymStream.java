package jcl.types;

import jcl.structs.packages.GlobalPackageStruct;
import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;

/**
 * A {@link SynonymStream} stream is a stream that is an alias for another stream, which is the value of a dynamic
 * variable whose name is the synonym stream symbol of the {@link SynonymStream}.
 * <p>
 * Any operations on a {@link SynonymStream} will be performed on the stream that is then the value of the dynamic
 * variable named by the synonym stream symbol. If the value of the variable should change, or if the variable should
 * be bound, then the stream will operate on the new value of the variable.
 * <p>
 * {@link SynonymStream} -> {@link Stream} -> {@link T}
 */
public interface SynonymStream extends Stream {

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
		 * Inner {@link SynonymStream} type implementation.
		 */
		private static final class SynonymStreamImpl extends TypeBaseClass implements SynonymStream, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private SynonymStreamImpl() {
				super("SYNONYM-STREAM", GlobalPackageStruct.COMMON_LISP);
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof SynonymStream);
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}
