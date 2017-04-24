/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import jcl.type.typespecifier.AtomicTypeSpecifier;

/**
 * A {@link TwoWayStreamType} is a bidirectional composite stream that receives its input from an associated input
 * stream and sends its output to an associated output stream.
 * <p>
 * {@link TwoWayStreamType} -&gt; {@link StreamType} -&gt; {@link TType}
 */
public interface TwoWayStreamType extends StreamType {

	/**
	 * Singleton instance of the {@link TwoWayStreamType} type.
	 */
	TwoWayStreamType INSTANCE = new Factory.TwoWayStreamTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<TwoWayStreamType> {

		@Override
		public TwoWayStreamType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link TwoWayStreamType} type implementation.
		 */
		private static final class TwoWayStreamTypeImpl extends TypeBaseClass implements TwoWayStreamType, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private TwoWayStreamTypeImpl() {
				super("TWO-WAY-STREAM");
			}

			@Override
			public boolean typeEquals(final Object obj) {
				return (this == obj) || (obj instanceof TwoWayStreamType);
			}
		}
	}
}
