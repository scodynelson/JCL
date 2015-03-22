/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;

/**
 * A {@link TwoWayStream} is a bidirectional composite stream that receives its input from an associated input stream
 * and sends its output to an associated output stream.
 * <p>
 * {@link TwoWayStream} -&gt; {@link Stream} -&gt; {@link T}
 */
public interface TwoWayStream extends Stream {

	/**
	 * Singleton instance of the {@link TwoWayStream} type.
	 */
	TwoWayStream INSTANCE = new Factory.TwoWayStreamImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<TwoWayStream> {

		@Override
		public TwoWayStream getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link TwoWayStream} type implementation.
		 */
		private static final class TwoWayStreamImpl extends TypeBaseClass implements TwoWayStream, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = -6594390772220907590L;

			/**
			 * Private constructor.
			 */
			private TwoWayStreamImpl() {
				super("TWO-WAY-STREAM");
			}
		}
	}
}
