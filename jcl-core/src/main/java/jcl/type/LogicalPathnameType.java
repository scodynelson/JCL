/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import jcl.type.typespecifier.AtomicTypeSpecifier;

/**
 * A {@link LogicalPathnameType} is a {@link PathnameType} that uses a namestring syntax and has component values.
 * {@link LogicalPathnameType}s do not refer directly to file names.
 * <p>
 * {@link LogicalPathnameType} -&gt; {@link PathnameType} -&gt; {@link TType}
 */
public interface LogicalPathnameType extends PathnameType {

	/**
	 * Singleton instance of the {@link LogicalPathnameType} type.
	 */
	LogicalPathnameType INSTANCE = new Factory.LogicalPathnameTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<LogicalPathnameType> {

		@Override
		public LogicalPathnameType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link LogicalPathnameType} type implementation.
		 */
		private static final class LogicalPathnameTypeImpl extends TypeBaseClass implements LogicalPathnameType, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private LogicalPathnameTypeImpl() {
				super("LOGICAL-PATHNAME");
			}

			@Override
			public boolean typeEquals(final Object obj) {
				return (this == obj) || (obj instanceof LogicalPathnameType);
			}
		}
	}
}
