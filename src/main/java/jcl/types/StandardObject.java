package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;

/**
 * The {@link StandardObject} is an instance of {@link StandardClass} and is a superclass of every {@link Class} that
 * is an instance of {@link StandardClass} except itself.
 * <p>
 * {@link StandardObject} -> {@link T}
 */
public interface StandardObject extends T {

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
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}
