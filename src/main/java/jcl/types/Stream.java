/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.String;

/**
 * A {@link Stream} is an object that can be used with an input or output function to identify an appropriate source or
 * sink of characters or bytes for that operation.
 * <p>
 * {@link Stream} -> {@link T}
 */
public interface Stream extends T {

	/**
	 * Singleton instance of the {@link Stream} type.
	 */
	Stream INSTANCE = new Factory.StreamImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<Stream> {

		@Override
		public Stream getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link Stream} type implementation.
		 */
		private static final class StreamImpl extends TypeBaseClass implements Stream, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 2426298092224091805L;

			/**
			 * Private constructor.
			 */
			private StreamImpl() {
				super("STREAM");
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof Stream);
			}

			@Override
			public String toString() {
//				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
				return getName();
			}
		}
	}
}
