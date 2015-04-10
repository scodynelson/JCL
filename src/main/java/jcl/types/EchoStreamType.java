/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * An {@link EchoStreamType} is a bidirectional composite stream that receives its input from an associated input
 * stream and sends its output to an associated output stream. All input taken from the input stream is echoed to the
 * output stream.
 * <p>
 * {@link EchoStreamType} -&gt; {@link StreamType} -&gt; {@link TType}
 */
public interface EchoStreamType extends StreamType {

	/**
	 * Singleton instance of the {@link EchoStreamType} type.
	 */
	EchoStreamType INSTANCE = new Factory.EchoStreamTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<EchoStreamType> {

		@Override
		public EchoStreamType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link EchoStreamType} type implementation.
		 */
		private static final class EchoStreamTypeImpl extends TypeBaseClass implements EchoStreamType, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = 35831157298596493L;

			/**
			 * Private constructor.
			 */
			private EchoStreamTypeImpl() {
				super("ECHO-STREAM");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof EchoStreamType);
			}
		}
	}
}
