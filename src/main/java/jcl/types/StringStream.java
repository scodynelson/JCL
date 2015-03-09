/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

import java.lang.String;

/**
 * A {@link StringStream} is a stream which reads input from or writes output to an associated {@link String}.
 * <p>
 * The stream element type of a {@link StringStream} is always a subtype of type {@link Character}.
 * <p>
 * {@link StringStream} -> {@link Stream} -> {@link T}
 */
public interface StringStream extends Stream {

	/**
	 * Singleton instance of the {@link StringStream} type.
	 */
	StringStream INSTANCE = new Factory.StringStreamImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<StringStream> {

		@Override
		public StringStream getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link StringStream} type implementation.
		 */
		private static final class StringStreamImpl extends TypeBaseClass implements StringStream, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = -4875583581598270616L;

			/**
			 * Private constructor.
			 */
			private StringStreamImpl() {
				super("STRING-STREAM");
			}

			@Override
			public int hashCode() {
				return HashCodeBuilder.reflectionHashCode(this);
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof StringStream);
			}

			@Override
			public String toString() {
//				return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
				return getName();
			}
		}
	}
}
