package jcl.types.packages;

import jcl.types.T;
import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code Package} is a namespace that maps symbol names to {@code Symbol}s.
 * <p/>
 * {@code Package} -> {@code T}
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
		 * Inner {@code Package} type implementation.
		 */
		private static class PackageImpl implements Package, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof Package)) {
					return false;
				}

				final Package aPackage = (Package) obj;
				return aPackage == INSTANCE;
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}
