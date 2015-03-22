/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import java.lang.String;

import jcl.types.typespecifiers.AtomicTypeSpecifier;

/**
 * A {@link StringStream} is a stream which reads input from or writes output to an associated {@link String}.
 * <p>
 * The stream element type of a {@link StringStream} is always a subtype of type {@link Character}.
 * <p>
 * {@link StringStream} -&gt; {@link Stream} -&gt; {@link T}
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
		}
	}
}
