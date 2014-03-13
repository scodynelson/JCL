package jcl.types;

import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * The type {@code Class} represents objects that determine the structure and behavior of their instances. Associated
 * with an object of type {@code Class} is information describing its place in the directed acyclic graph of classes,
 * its slots, and its options.
 * <p/>
 * {@code Class} -> {@code StandardObject} -> {@code T}
 */
public interface Class extends StandardObject {

	Class INSTANCE = new Factory.ClassImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Class> {

		@Override
		public Class getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@code Class} type implementation.
		 */
		private static class ClassImpl implements Class, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof Class);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}
