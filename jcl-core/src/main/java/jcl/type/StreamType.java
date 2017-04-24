/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import jcl.type.typespecifier.AtomicTypeSpecifier;

/**
 * A {@link StreamType} is an object that can be used with an input or output function to identify an appropriate
 * source or sink of characters or bytes for that operation.
 * <p>
 * {@link StreamType} -&gt; {@link TType}
 */
public interface StreamType extends TType {

	/**
	 * Singleton instance of the {@link StreamType} type.
	 */
	StreamType INSTANCE = new Factory.StreamTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<StreamType> {

		@Override
		public StreamType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link StreamType} type implementation.
		 */
		private static final class StreamTypeImpl extends TypeBaseClass implements StreamType, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private StreamTypeImpl() {
				super("STREAM");
			}

			@Override
			public boolean typeEquals(final Object obj) {
				return (this == obj) || (obj instanceof StreamType);
			}
		}
	}
}
