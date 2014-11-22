package jcl.types;

import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.lang.String;

/**
 * The type {@link NIL} contains no objects and so is also called the empty type. The type {@link NIL} is a subtype of
 * every type. No object is of type {@link NIL}.
 * <p>
 * The type containing the object {@link NIL} is the type {@link Null}, not the type {@link NIL}.
 * <p>
 * {@link NIL}
 */
public interface NIL extends T { // TODO: this needs to extend ALL types...

	NIL INSTANCE = new Factory.NILImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<NIL> {

		@Override
		public NIL getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link NIL} type implementation.
		 */
		private static final class NILImpl extends TypeBaseClass implements NIL, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private NILImpl() {
				super("NIL");
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof NIL) || (obj instanceof Null);
			}

			@Override
			public String toString() {
				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
			}
		}
	}
}
