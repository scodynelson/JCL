/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import jcl.type.typespecifier.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link SynonymStreamType} stream is a stream that is an alias for another stream, which is the value of a dynamic
 * variable whose name is the synonym stream symbol of the {@link SynonymStreamType}.
 * <p>
 * Any operations on a {@link SynonymStreamType} will be performed on the stream that is then the value of the dynamic
 * variable named by the synonym stream symbol. If the value of the variable should change, or if the variable should
 * be bound, then the stream will operate on the new value of the variable.
 * <p>
 * {@link SynonymStreamType} -&gt; {@link StreamType} -&gt; {@link TType}
 */
public interface SynonymStreamType extends StreamType {

	/**
	 * Singleton instance of the {@link SynonymStreamType} type.
	 */
	SynonymStreamType INSTANCE = new Factory.SynonymStreamTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<SynonymStreamType> {

		@Override
		public SynonymStreamType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link SynonymStreamType} type implementation.
		 */
		private static final class SynonymStreamTypeImpl extends TypeBaseClass implements SynonymStreamType, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private SynonymStreamTypeImpl() {
				super("SYNONYM-STREAM");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof SynonymStreamType);
			}
		}
	}
}
