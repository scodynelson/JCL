/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.type;

import jcl.type.typespecifier.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * The only object of type {@link NullType} is {@link NILType}, which represents the empty {@link ListType} and can
 * also be notated ().
 * <p>
 * {@link NullType} -&gt; {@link SymbolType} -&gt; {@link ListType} -&gt; {@link SequenceType} -&gt; {@link TType}
 */
public interface NullType extends SymbolType, ListType {

	/**
	 * Singleton instance of the {@link NullType} type.
	 */
	NullType INSTANCE = new Factory.NullTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<NullType> {

		@Override
		public NullType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link NullType} type implementation.
		 */
		private static final class NullTypeImpl extends TypeBaseClass implements NullType, AtomicTypeSpecifier {

			/**
			 * Private constructor.
			 */
			private NullTypeImpl() {
				super("NULL");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof NullType);
			}
		}
	}
}
