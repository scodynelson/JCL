package jcl.types;

import jcl.structs.packages.GlobalPackageStruct;
import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.String;

/**
 * A {@link StructureClass} is the default structure type {@link Class}.
 * <p>
 * {@link StructureClass} -> {@link Class} -> {@link StandardObject} -> {@link T}
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
		 * Inner {@link StructureClass} type implementation.
		 */
		private static class StructureClassImpl extends TypeBaseClass implements StructureClass, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private StructureClassImpl() {
				super("STRUCTURE-CLASS", GlobalPackageStruct.COMMON_LISP);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof StructureClass);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}

			@Override
			public String toString() {
				return "StructureClassImpl{}";
			}
		}
	}
}
