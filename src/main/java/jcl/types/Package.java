package jcl.types;

import jcl.structs.packages.GlobalPackageStruct;
import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.String;

/**
 * A {@link Package} is a namespace that maps symbol names to {@link Symbol}s.
 * <p>
 * {@link Package} -> {@link T}
 */
public interface Package extends T {

	Package INSTANCE = new Factory.PackageImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Package> {

		@Override
		public Package getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Package} type implementation.
		 */
		private static class PackageImpl extends TypeBaseClass implements Package, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private PackageImpl() {
				super("PACKAGE", GlobalPackageStruct.COMMON_LISP);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof Package);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}

			@Override
			public String toString() {
				return "PackageImpl{}";
			}
		}
	}
}
