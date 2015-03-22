/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link SynonymStream} stream is a stream that is an alias for another stream, which is the value of a dynamic
 * variable whose name is the synonym stream symbol of the {@link SynonymStream}.
 * <p>
 * Any operations on a {@link SynonymStream} will be performed on the stream that is then the value of the dynamic
 * variable named by the synonym stream symbol. If the value of the variable should change, or if the variable should
 * be bound, then the stream will operate on the new value of the variable.
 * <p>
 * {@link SynonymStream} -&gt; {@link Stream} -&gt; {@link T}
 */
public interface SynonymStream extends Stream {

	/**
	 * Singleton instance of the {@link SynonymStream} type.
	 */
	SynonymStream INSTANCE = new Factory.SynonymStreamImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<SynonymStream> {

		@Override
		public SynonymStream getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link SynonymStream} type implementation.
		 */
		private static final class SynonymStreamImpl extends TypeBaseClass implements SynonymStream, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = -6782441795090632210L;

			/**
			 * Private constructor.
			 */
			private SynonymStreamImpl() {
				super("SYNONYM-STREAM");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof SynonymStream);
			}
		}
	}
}
