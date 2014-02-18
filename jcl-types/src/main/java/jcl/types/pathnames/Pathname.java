package jcl.types.pathnames;

import jcl.types.T;
import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code Pathname} is a structured object which represents a filename.
 * <p/>
 * There are two kinds of {@code Pathname}s---physical pathnames and {@code LogicalPathname}s.
 * <p/>
 * {@code Pathname} -> {@code T}
 */
public interface Pathname extends T {

	Pathname INSTANCE = new Factory.PathnameImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Pathname> {

		@Override
		public Pathname getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@code Pathname} type implementation.
		 */
		private static class PathnameImpl implements Pathname, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				if (this == obj) {
					return true;
				}

				if (!(obj instanceof Pathname)) {
					return false;
				}

				final Pathname pathname = (Pathname) obj;
				return pathname == INSTANCE;
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}
