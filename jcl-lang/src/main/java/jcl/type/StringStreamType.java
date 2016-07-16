/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import jcl.type.typespecifier.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link StringStreamType} is a stream which reads input from or writes output to an associated {@link String}.
 * <p>
 * The stream element type of a {@link StringStreamType} is always a subtype of type {@link CharacterType}.
 * <p>
 * {@link StringStreamType} -&gt; {@link StreamType} -&gt; {@link TType}
 */
public interface StringStreamType extends StreamType {

	/**
	 * Singleton instance of the {@link StringStreamType} type.
	 */
	StringStreamType INSTANCE = new Factory.StringStreamTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<StringStreamType> {

		@Override
		public StringStreamType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link StringStreamType} type implementation.
		 */
		private static final class StringStreamTypeImpl extends TypeBaseClass implements StringStreamType, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private StringStreamTypeImpl() {
				super("STRING-STREAM");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof StringStreamType);
			}
		}
	}
}
