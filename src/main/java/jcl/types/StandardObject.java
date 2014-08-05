package jcl.types;

import jcl.structs.packages.GlobalPackageStruct;
import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

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
		private static class StandardObjectImpl extends TypeBaseClass implements StandardObject, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private StandardObjectImpl() {
				super("STANDARD-OBJECT", GlobalPackageStruct.COMMON_LISP);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof StandardObject);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}

			@Override
			public String toString() {
				return "StandardObjectImpl{}";
			}
		}
	}
}
