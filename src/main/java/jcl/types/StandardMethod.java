package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;

/**
 * A {@link StandardMethod} is the default method type {@link Class}.
 * <p>
 * {@link StandardMethod} -> {@link Method} -> {@link StandardObject} -> {@link T}
 */
public interface StandardMethod extends Method, StandardObject {

	StandardMethod INSTANCE = new Factory.StandardMethodImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<StandardMethod> {

		@Override
		public StandardMethod getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link StandardMethod} type implementation.
		 */
		private static final class StandardMethodImpl extends TypeBaseClass implements StandardMethod, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private StandardMethodImpl() {
				super("STANDARD-METHOD");
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof StandardMethod);
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}
