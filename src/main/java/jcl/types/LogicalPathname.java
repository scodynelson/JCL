/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.String;

/**
 * A {@link LogicalPathname} is a {@link Pathname} that uses a namestring syntax and has component values.
 * {@link LogicalPathname}s do not refer directly to file names.
 * <p>
 * {@link LogicalPathname} -&gt; {@link Pathname} -&gt; {@link T}
 */
public interface LogicalPathname extends Pathname {

	/**
	 * Singleton instance of the {@link LogicalPathname} type.
	 */
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
		private static final class LogicalPathnameImpl extends TypeBaseClass implements LogicalPathname, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 8248360316484756067L;

			/**
			 * Private constructor.
			 */
			private LogicalPathnameImpl() {
				super("LOGICAL-PATHNAME");
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof LogicalPathname);
			}

			@Override
			public String toString() {
//				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
				return getName();
			}
		}
	}
}
