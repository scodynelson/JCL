package jcl.types.classes;

import jcl.types.T;
import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code StructureClass} is the default structure type {@code Class}.
 */
public interface StructureClass extends Class, StandardObject, T {

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
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof StructureClass)) {
					return false;
				}

				final StructureClass structureClass = (StructureClass) obj;
				return structureClass == INSTANCE;
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}
