/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * An {@link EchoStream} is a bidirectional composite stream that receives its input from an associated input stream
 * and sends its output to an associated output stream. All input taken from the input stream is echoed to the output
 * stream.
 * <p>
 * {@link EchoStream} -&gt; {@link Stream} -&gt; {@link T}
 */
public interface EchoStream extends Stream {

	/**
	 * Singleton instance of the {@link EchoStream} type.
	 */
	EchoStream INSTANCE = new Factory.EchoStreamImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<EchoStream> {

		@Override
		public EchoStream getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link EchoStream} type implementation.
		 */
		private static final class EchoStreamImpl extends TypeBaseClass implements EchoStream, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 35831157298596493L;

			/**
			 * Private constructor.
			 */
			private EchoStreamImpl() {
				super("ECHO-STREAM");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof EchoStream);
			}
		}
	}
}
