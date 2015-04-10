/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.types;

import jcl.types.typespecifiers.AtomicTypeSpecifier;
import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * A {@link ReadtableType} maps characters into syntax types for the Lisp reader. A {@link ReadtableType} also contains
 * associations between macro characters and their reader macro functions, and records information about the case
 * conversion rules to be used by the Lisp reader when parsing {@link SymbolType}s.
 * <p>
 * Each simple {@link CharacterType} must be representable in the {@link ReadtableType}.
 * <p>
 * {@link ReadtableType} -&gt; {@link TType}
 */
public interface ReadtableType extends TType {

	/**
	 * Singleton instance of the {@link ReadtableType} type.
	 */
	ReadtableType INSTANCE = new Factory.ReadtableTypeImpl();

	/**
	 * Factory.
	 */
	class Factory implements TypeFactory<ReadtableType> {

		@Override
		public ReadtableType getInstance() {
			return INSTANCE;
		}

		/**
		 * Inner {@link ReadtableType} type implementation.
		 */
		private static final class ReadtableTypeImpl extends TypeBaseClass implements ReadtableType, AtomicTypeSpecifier {

			/**
			 * Serializable Version Unique Identifier.
			 */
			private static final long serialVersionUID = -9157541006854981003L;

			/**
			 * Private constructor.
			 */
			private ReadtableTypeImpl() {
				super("READTABLE");
			}

			@Override
			public int hashCode() {
				return new HashCodeBuilder().appendSuper(super.hashCode())
				                            .toHashCode();
			}

			@Override
			public boolean equals(final Object obj) {
				return (this == obj) || (obj instanceof ReadtableType);
			}
		}
	}
}
