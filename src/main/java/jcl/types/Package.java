package jcl.types;

import jcl.typespecifiers.AtomicTypeSpecifier;
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
				return (this == obj) || (obj instanceof Package);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}
