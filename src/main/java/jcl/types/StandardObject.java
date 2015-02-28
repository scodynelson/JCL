package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.String;

/**
 * The {@link StandardObject} is an instance of {@link StandardClass} and is a superclass of every {@link Class} that
 * is an instance of {@link StandardClass} except itself.
 * <p>
 * {@link StandardObject} -> {@link T}
 */
public interface StandardObject extends T {

	/**
	 * Singleton instance of the {@link StandardObject} type.
	 */
	StandardObject INSTANCE = new Factory.StandardObjectImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<StandardObject> {

		@Override
		public StandardObject getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link StandardObject} type implementation.
		 */
		private static final class StandardObjectImpl extends TypeBaseClass implements StandardObject, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = -3398435200495623399L;

			/**
			 * Private constructor.
			 */
			private StandardObjectImpl() {
				super("STANDARD-OBJECT");
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof StandardObject);
			}

			@Override
			public String toString() {
//				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
				return getName();
			}
		}
	}
}
