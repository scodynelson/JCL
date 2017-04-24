/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import jcl.type.typespecifier.AtomicTypeSpecifier;

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
			 * Private constructor.
			 */
			private EchoStreamTypeImpl() {
				super("ECHO-STREAM");
			}

			@Override
			public boolean typeEquals(final Object obj) {
				return (this == obj) || (obj instanceof EchoStreamType);
			}
		}
	}
}
