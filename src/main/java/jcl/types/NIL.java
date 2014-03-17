package jcl.types;

import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * The type {@code NIL} contains no objects and so is also called the empty type. The type {@code NIL} is a subtype of
 * every type. No object is of type {@code NIL}.
 * <p/>
 * The type containing the object {@code NIL} is the type {@code Null}, not the type {@code NIL}.
 * <p/>
 * {@code NIL}
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
		 * Inner {@code NIL} type implementation.
		 */
		private static class NILImpl implements NIL, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof NIL) || (obj instanceof Null); //NOSONAR
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}
