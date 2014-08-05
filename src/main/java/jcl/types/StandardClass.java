package jcl.types;

import jcl.structs.packages.GlobalPackageStruct;
import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.String;

/**
 * A {@link StandardClass} is the default class type {@link Class}.
 * <p>
 * {@link StandardClass} -> {@link Class} -> {@link StandardObject} -> {@link T}
 */
public interface StandardClass extends Class {

	StandardClass INSTANCE = new Factory.StandardClassImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<StandardClass> {

		@Override
		public StandardClass getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link StandardClass} type implementation.
		 */
		private static class StandardClassImpl extends TypeBaseClass implements StandardClass, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private StandardClassImpl() {
				super("STANDARD-CLASS", GlobalPackageStruct.COMMON_LISP);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof StandardClass);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}

			@Override
			public String toString() {
				return "StandardClassImpl{}";
			}
		}
	}
}
