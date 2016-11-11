/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import jcl.type.typespecifier.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link BroadcastStreamType} is an output stream which which is a composite stream of zero or more other output
 * streams, such that any output sent to the {@link BroadcastStreamType} gets passed on as output to each of the
 * associated output streams.
 * <p>
 * If a {@link BroadcastStreamType} has no component streams, then all output to the {@link BroadcastStreamType} is
 * discarded.
 * <p>
 * The set of operations that may be performed on a broadcast stream is the intersection of those for its associated
 * output streams.
 * <p>
 * {@link BroadcastStreamType} -&gt; {@link StreamType} -&gt; {@link TType}
 */
public interface BroadcastStreamType extends StreamType {

	/**
	 * Singleton instance of the {@link BroadcastStreamType} type.
	 */
	BroadcastStreamType INSTANCE = new Factory.BroadcastStreamTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<BroadcastStreamType> {

		@Override
		public BroadcastStreamType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link BroadcastStreamType} type implementation.
		 */
		private static final class BroadcastStreamTypeImpl extends TypeBaseClass implements BroadcastStreamType, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private BroadcastStreamTypeImpl() {
				super("BROADCAST-STREAM");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof BroadcastStreamType);
			}
		}
	}
}
