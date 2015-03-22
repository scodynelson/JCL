/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;

/**
 * A {@link BroadcastStream} is an output stream which which is a composite stream of zero or more other output
 * streams, such that any output sent to the {@link BroadcastStream} gets passed on as output to each of the associated
 * output streams.
 * <p>
 * If a {@link BroadcastStream} has no component streams, then all output to the {@link BroadcastStream} is discarded.
 * <p>
 * The set of operations that may be performed on a broadcast stream is the intersection of those for its associated
 * output streams.
 * <p>
 * {@link BroadcastStream} -&gt; {@link Stream} -&gt; {@link T}
 */
public interface BroadcastStream extends Stream {

	/**
	 * Singleton instance of the {@link BroadcastStream} type.
	 */
	BroadcastStream INSTANCE = new Factory.BroadcastStreamImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<BroadcastStream> {

		@Override
		public BroadcastStream getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link BroadcastStream} type implementation.
		 */
		private static final class BroadcastStreamImpl extends TypeBaseClass implements BroadcastStream, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 4069710741454263582L;

			/**
			 * Private constructor.
			 */
			private BroadcastStreamImpl() {
				super("BROADCAST-STREAM");
			}
		}
	}
}
