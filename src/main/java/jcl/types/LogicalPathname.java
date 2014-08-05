package jcl.types;

import jcl.structs.packages.GlobalPackageStruct;
import jcl.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.String;

/**
 * A {@link LogicalPathname} is a {@link Pathname} that uses a namestring syntax and has component values.
 * {@link LogicalPathname}s do not refer directly to file names.
 * <p>
 * {@link LogicalPathname} -> {@link Pathname} -> {@link T}
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
		 * Inner {@link LogicalPathname} type implementation.
		 */
		private static class LogicalPathnameImpl extends TypeBaseClass implements LogicalPathname, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private LogicalPathnameImpl() {
				super("LOGICAL-PATHNAME", GlobalPackageStruct.COMMON_LISP);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof LogicalPathname);
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().toHashCode();
			}

			@Override
			public String toString() {
				return "LogicalPathnameImpl{}";
			}
		}
	}
}
