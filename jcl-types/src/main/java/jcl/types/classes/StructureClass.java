package jcl.types.classes;

import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code StructureClass} is the default structure type {@code Class}.
 * <p/>
 * {@code StructureClass} -> {@code Class} -> {@code StandardObject} -> {@code T}
 */
public interface StructureClass extends Class {

	StructureClass INSTANCE = new Factory.StructureClassImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<StructureClass> {

		@Override
		public StructureClass getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@code StructureClass} type implementation.
		 */
		private static class StructureClassImpl implements StructureClass, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof StructureClass);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}
