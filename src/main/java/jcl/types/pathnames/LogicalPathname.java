package jcl.types.pathnames;

import jcl.types.TypeFactory;
import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@code LogicalPathname} is a {@code Pathname} that uses a namestring syntax and has component values.
 * {@code LogicalPathname}s do not refer directly to file names.
 * <p/>
 * {@code LogicalPathname} -> {@code Pathname} -> {@code T}
 */
public interface LogicalPathname extends Pathname {

	LogicalPathname INSTANCE = new Factory.LogicalPathnameImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<LogicalPathname> {

		@Override
		public LogicalPathname getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@code LogicalPathname} type implementation.
		 */
		private static class LogicalPathnameImpl implements LogicalPathname, AtomicTypeSpecifier {

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof LogicalPathname);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}
		}
	}
}
