package jcl.types;

import jcl.structs.packages.GlobalPackageStruct;
import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.String;

/**
 * A {@link StandardMethod} is the default method type {@link Class}.
 * <p>
 * {@link StandardMethod} -> {@link Method} -> {@link StandardObject} -> {@link T}
 */
public interface StandardMethod extends Method, StandardObject {

	StandardMethod INSTANCE = new Factory.StandardMethodImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<StandardMethod> {

		@Override
		public StandardMethod getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link StandardMethod} type implementation.
		 */
		private static class StandardMethodImpl extends TypeBaseClass implements StandardMethod, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private StandardMethodImpl() {
				super("STANDARD-METHOD", GlobalPackageStruct.COMMON_LISP);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof StandardMethod);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}

			@Override
			public String toString() {
				return "StandardMethodImpl{}";
			}
		}
	}
}
