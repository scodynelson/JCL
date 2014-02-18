package jcl.types.classes;

import jcl.types.T;
import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * The {@code StructureObject} is an instance of {@code StructureClass} and is a superclass of every {@code Class} that
 * is an instance of {@code StructureClass} except itself.
 * <p/>
 * {@code StructureObject} -> {@code T}
 */
public interface StructureObject extends T {

	StructureObject INSTANCE = new Factory.StructureObjectImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<StructureObject> {

		@Override
		public StructureObject getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@code StructureObject} type implementation.
		 */
		private static class StructureObjectImpl implements StructureObject, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof StructureObject)) {
					return false;
				}

				final StructureObject structureObject = (StructureObject) obj;
				return structureObject == INSTANCE;
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}
