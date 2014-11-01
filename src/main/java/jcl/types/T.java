package jcl.types;

import jcl.LispType;
import jcl.structs.packages.GlobalPackageStruct;
import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;

/**
 * The type {@link T} is the set of all objects. It is a supertype of every type, including itself. Every object is of
 * type {@link T}.
 * <p>
 * {@link T}
 */
public interface T extends LispType {

	T INSTANCE = new Factory.TImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<T> {

		@Override
		public T getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link T} type implementation.
		 */
		private static class TImpl extends TypeBaseClass implements T, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private TImpl() {
				super("T", GlobalPackageStruct.COMMON_LISP);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof T);
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
